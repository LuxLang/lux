(ns lux.compiler.js.rt
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return |let |case]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [optimizer :as &o]
                 [host :as &host])
            [lux.analyser.base :as &a]
            [lux.compiler.js.base :as &&]))

(def ^:private const-none (str "[0,null," &&/unit "]"))
(defn ^:private make-some [value]
  (str "[1,''," value "]"))

(def ^:private adt-methods
  {"product_getLeft" (str "(function LuxRT$product_getLeft(product,index) {"
                          "var index_min_length = (index+1);"
                          "if(product.length > index_min_length) {"
                          ;; No need for recursion
                          "return product[index];"
                          "}"
                          "else {"
                          ;; Needs recursion
                          "return LuxRT$product_getLeft(product[product.length - 1], (index_min_length - product.length));"
                          "}"
                          "})")
   "product_getRight" (str "(function LuxRT$product_getRight(product,index) {"
                           "var index_min_length = (index+1);"
                           "if(product.length === index_min_length) {"
                           ;; Last element.
                           "return product[index];"
                           "}"
                           "else if(product.length < index_min_length) {"
                           ;; Needs recursion
                           "return LuxRT$product_getRight(product[product.length - 1], (index_min_length - product.length));"
                           "}"
                           "else {"
                           ;; Must slice
                           "return product.slice(index);"
                           "}"
                           "})")
   "sum_get" (let [no-match "return null;"
                   extact-match "return sum[2];"
                   recursion-test (str (str "if(sum[1] === '') {"
                                            ;; Must recurse.
                                            "return LuxRT$sum_get(sum[2], (wantedTag - sum[0]), wantsLast);"
                                            "}"
                                            "else { " no-match " }"))]
               (str "(function LuxRT$sum_get(sum,wantedTag,wantsLast) {"
                    "if(wantedTag === sum[0]) {"
                    (str "if(sum[1] === wantsLast) {" extact-match "}"
                         "else {" recursion-test "}")
                    "}"
                    (str "else if(wantedTag > sum[0]) {" recursion-test "}")
                    (str "else if(wantedTag < sum[0] && wantsLast === '') {"
                         "return [(sum[0]-wantedTag),sum[1],sum[2]];"
                         "}")
                    "else { " no-match " }"
                    "})"))
   })

(def ^:private i64-methods
  {"TWO_PWR_16" "(1 << 16)"
   "TWO_PWR_32" "((1 << 16) * (1 << 16))"
   "TWO_PWR_64" "(((1 << 16) * (1 << 16)) * ((1 << 16) * (1 << 16)))"
   "TWO_PWR_63" "((((1 << 16) * (1 << 16)) * ((1 << 16) * (1 << 16))) / 2)"
   "getLowBitsUnsigned" (str "(function LuxRT$getLowBitsUnsigned(i64) {"
                             "return (i64.L >= 0) ? i64.L : (LuxRT$TWO_PWR_32 + i64.L);"
                             "})")
   "toNumberI64" (str "(function LuxRT$toNumberI64(i64) {"
                      "return (i64.H * LuxRT$TWO_PWR_32) + LuxRT$getLowBitsUnsigned(i64);"
                      "})")
   "fromNumberI64" (str "(function LuxRT$fromNumberI64(num) {"
                        (str "if(isNaN(num)) {"
                             "return LuxRT$ZERO;"
                             "}")
                        (str "else if(num <= -LuxRT$TWO_PWR_63) {"
                             "return LuxRT$MIN_VALUE_I64;"
                             "}")
                        (str "else if((num + 1) >= LuxRT$TWO_PWR_63) {"
                             "return LuxRT$MAX_VALUE_I64;"
                             "}")
                        (str "else if(num < 0) {"
                             "return LuxRT$negateI64(LuxRT$fromNumberI64(-num));"
                             "}")
                        (str "else {"
                             "return LuxRT$makeI64((num / LuxRT$TWO_PWR_32), (num % LuxRT$TWO_PWR_32));"
                             "}")
                        "})")
   "makeI64" (str "(function LuxRT$makeI64(high,low) {"
                  "return { H: (high|0), L: (low|0)};"
                  "})")
   "MIN_VALUE_I64" "{ H: (0x80000000|0), L: (0|0)}"
   "MAX_VALUE_I64" "{ H: (0x7FFFFFFF|0), L: (0xFFFFFFFF|0)}"
   "ONE" "{ H: (0|0), L: (1|0)}"
   "ZERO" "{ H: (0|0), L: (0|0)}"
   "notI64" (str "(function LuxRT$notI64(i64) {"
                 "return LuxRT$makeI64(~i64.H,~i64.L);"
                 "})")
   "negateI64" (str "(function LuxRT$negateI64(i64) {"
                    (str "if(LuxRT$eqI64(LuxRT$MIN_VALUE_I64,i64)) {"
                         "return LuxRT$MIN_VALUE_I64;"
                         "}")
                    (str "else {"
                         "return LuxRT$addI64(LuxRT$notI64(i64),LuxRT$ONE);"
                         "}")
                    "})")
   "eqI64" (str "(function LuxRT$eqI64(l,r) {"
                "return (l.H === r.H) && (l.L === r.L);"
                "})")
   "addI64" (str "(function LuxRT$addI64(l,r) {"
                 "var l48 = l.H >>> 16;"
                 "var l32 = l.H & 0xFFFF;"
                 "var l16 = l.L >>> 16;"
                 "var l00 = l.L & 0xFFFF;"

                 "var r48 = r.H >>> 16;"
                 "var r32 = r.H & 0xFFFF;"
                 "var r16 = r.L >>> 16;"
                 "var r00 = r.L & 0xFFFF;"

                 "var x48 = 0, x32 = 0, x16 = 0, x00 = 0;"
                 "x00 += l00 + r00;"
                 "x16 += x00 >>> 16;"
                 "x00 &= 0xFFFF;"
                 "x16 += l16 + r16;"
                 "x32 += x16 >>> 16;"
                 "x16 &= 0xFFFF;"
                 "x32 += l32 + r32;"
                 "x48 += x32 >>> 16;"
                 "x32 &= 0xFFFF;"
                 "x48 += l48 + r48;"
                 "x48 &= 0xFFFF;"

                 "return LuxRT$makeI64((x48 << 16) | x32, (x16 << 16) | x00);"
                 "})")
   "subI64" (str "(function LuxRT$subI64(l,r) {"
                 "return LuxRT$addI64(l,LuxRT$negateI64(r));"
                 "})")
   "mulI64" (str "(function LuxRT$mulI64(l,r) {"
                 "if (l.H < 0) {"
                 (str "if (r.H < 0) {"
                      ;; Both are negative
                      "return LuxRT$mulI64(LuxRT$negateI64(l),LuxRT$negateI64(r));"
                      "}"
                      "else {"
                      ;; Left is negative
                      "return LuxRT$negateI64(LuxRT$mulI64(LuxRT$negateI64(l),r));"
                      "}")
                 "}"
                 "else if (r.H < 0) {"
                 ;; Right is negative
                 "return LuxRT$negateI64(LuxRT$mulI64(l,LuxRT$negateI64(r)));"
                 "}"
                 ;; Both are positive
                 "else {"
                 "var l48 = l.H >>> 16;"
                 "var l32 = l.H & 0xFFFF;"
                 "var l16 = l.L >>> 16;"
                 "var l00 = l.L & 0xFFFF;"

                 "var r48 = r.H >>> 16;"
                 "var r32 = r.H & 0xFFFF;"
                 "var r16 = r.L >>> 16;"
                 "var r00 = r.L & 0xFFFF;"

                 "var x48 = 0, x32 = 0, x16 = 0, x00 = 0;"
                 "x00 += l00 * r00;"
                 "x16 += x00 >>> 16;"
                 "x00 &= 0xFFFF;"
                 "x16 += l16 * r00;"
                 "x32 += x16 >>> 16;"
                 "x16 &= 0xFFFF;"
                 "x16 += l00 * r16;"
                 "x32 += x16 >>> 16;"
                 "x16 &= 0xFFFF;"
                 "x32 += l32 * r00;"
                 "x48 += x32 >>> 16;"
                 "x32 &= 0xFFFF;"
                 "x32 += l16 * r16;"
                 "x48 += x32 >>> 16;"
                 "x32 &= 0xFFFF;"
                 "x32 += l00 * r32;"
                 "x48 += x32 >>> 16;"
                 "x32 &= 0xFFFF;"
                 "x48 += (l48 * r00) + (l32 * r16) + (l16 * r32) + (l00 * r48);"
                 "x48 &= 0xFFFF;"

                 "return LuxRT$makeI64((x48 << 16) | x32, (x16 << 16) | x00);"
                 "}"
                 "})")
   "divI64" (str "(function LuxRT$divI64(l,r) {"
                 (str "if((r.H === 0) && (r.L === 0)) {"
                      ;; Special case: R = 0
                      "throw new Error('Cannot divide by zero!');"
                      "}"
                      "else if((l.H === 0) && (l.L === 0)) {"
                      ;; Special case: L = 0
                      "return l;"
                      "}")
                 (str "if(LuxRT$eqI64(l,LuxRT$MIN_VALUE_I64)) {"
                      ;; Special case: L = MIN
                      (str "if(LuxRT$eqI64(r,LuxRT$ONE) || LuxRT$eqI64(r,LuxRT$negateI64(LuxRT$ONE))) {"
                           ;; Special case: L = MIN, R = 1|-1
                           "return LuxRT$MIN_VALUE_I64;"
                           "}"
                           ;; Special case: L = R = MIN
                           "else if(LuxRT$eqI64(r,LuxRT$MIN_VALUE_I64)) {"
                           "return LuxRT$ONE;"
                           "}"
                           ;; Special case: L = MIN
                           "else {"
                           "var halfL = LuxRT$shrI64(l,1);"
                           "var approx = LuxRT$shlI64(LuxRT$divI64(halfL,r),LuxRT$ONE);"
                           (str "if((approx.H === 0) && (approx.L === 0)) {"
                                (str "if(r.H < 0) {"
                                     "return LuxRT$ONE;"
                                     "}"
                                     "else {"
                                     "return LuxRT$negateI64(LuxRT$ONE);"
                                     "}")
                                "}"
                                "else {"
                                "var rem = LuxRT$subI64(l,LuxRT$mulI64(r,approx));"
                                "return LuxRT$addI64(approx,LuxRT$divI64(rem,r));"
                                "}")
                           "}")
                      "}"
                      "else if(LuxRT$eqI64(r,LuxRT$MIN_VALUE_I64)) {"
                      ;; Special case: R = MIN
                      "return LuxRT$makeI64(0,0);"
                      "}")
                 ;; Special case: negatives
                 (str "if(l.H < 0) {"
                      (str "if(r.H < 0) {"
                           ;; Both are negative
                           "return LuxRT$divI64(LuxRT$negateI64(l),LuxRT$negateI64(r));"
                           "}"
                           "else {"
                           ;; Only L is negative
                           "return LuxRT$negateI64(LuxRT$divI64(LuxRT$negateI64(l),r));"
                           "}")
                      "}"
                      "else if(r.H < 0) {"
                      ;; R is negative
                      "return LuxRT$negateI64(LuxRT$divI64(l,LuxRT$negateI64(r)));"
                      "}")
                 ;; Common case
                 (str "var res = LuxRT$ZERO;"
                      "var rem = l;"
                      (str "while(LuxRT$ltI64(r,rem) || LuxRT$eqI64(r,rem)) {"
                           "var approx = Math.max(1, Math.floor(LuxRT$toNumberI64(rem) / LuxRT$toNumberI64(r)));"
                           "var log2 = Math.ceil(Math.log(approx) / Math.LN2);"
                           "var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);"
                           "var approxRes = LuxRT$fromNumberI64(approx);"
                           "var approxRem = LuxRT$mulI64(approxRes,r);"
                           (str "while((approxRem.H < 0) || LuxRT$ltI64(rem,approxRem)) {"
                                "approx -= delta;"
                                "approxRes = LuxRT$fromNumberI64(approx);"
                                "approxRem = LuxRT$mulI64(approxRes,r);"
                                "}")
                           (str "if((approxRes.H === 0) && (approxRes.L === 0)) {"
                                "approxRes = LuxRT$ONE;"
                                "}")
                           "res = LuxRT$addI64(res,approxRes);"
                           "rem = LuxRT$subI64(rem,approxRem);"
                           "}")
                      "return res;")
                 "})")
   "remI64" (str "(function LuxRT$remI64(l,r) {"
                 "return LuxRT$subI64(l,LuxRT$mulI64(LuxRT$divI64(l,r),r));"
                 "})")
   "ltI64" (str "(function LuxRT$ltI64(l,r) {"
                "var ln = l.H < 0;"
                "var rn = r.H < 0;"
                "if(ln && !rn) { return true; }"
                "if(!ln && rn) { return false; }"
                "return (LuxRT$subI64(l,r).H < 0);"
                "})")
   "encodeI64" (str "(function LuxRT$encodeI64(input) {"
                    ;; If input = 0
                    (str "if((input.H === 0) && (input.L === 0)) {"
                         "return '0';"
                         "}")
                    ;; If input < 0
                    (str "if(input.H < 0) {"
                         (str "if(LuxRT$eqI64(input,LuxRT$MIN_VALUE_I64)) {"
                              "var radix = LuxRT$makeI64(0,10);"
                              "var div = LuxRT$divI64(input,radix);"
                              "var rem = LuxRT$subI64(LuxRT$mulI64(div,radix),input);"
                              "return LuxRT$encodeI64(div).concat(rem.L+'');"
                              "}")
                         (str "else {"
                              "return '-'.concat(LuxRT$encodeI64(LuxRT$negateI64(input)));"
                              "}")
                         "}")
                    ;; If input > 0
                    (str "var chunker = LuxRT$makeI64(0,1000000);"
                         "var rem = input;"
                         "var result = '';"
                         "while(true) {"
                         (str "var remDiv = LuxRT$divI64(rem,chunker);"
                              "var chunk = LuxRT$subI64(rem,LuxRT$mulI64(remDiv,chunker));"
                              "var digits = (chunk.L >>> 0)+'';"
                              "rem = remDiv;"
                              (str "if((rem.H === 0) && (rem.L === 0)) {"
                                   "return digits.concat(result);"
                                   "}"
                                   "else {"
                                   (str "while(digits.length < 6) {"
                                        "digits = '0' + digits;"
                                        "}")
                                   "result = '' + digits + result;"
                                   "}"))
                         "}")
                    "})")
   "decodeI64" (str "(function LuxRT$decodeI64(input) {"
                    "input = LuxRT$clean_separators(input);"
                    (str "if(/^-?\\d+$/.exec(input)) {"
                         (str "var isNegative = (input.charAt(0) == '-');"
                              "var sign = isNegative ? -1 : 1;"
                              "input = isNegative ? input.substring(1) : input;"

                              "var chunkPower = LuxRT$fromNumberI64(Math.pow(10, 8));"
                              "var result = LuxRT$ZERO;"
                              (str "for (var i = 0; i < input.length; i += 8) {"
                                   "var size = Math.min(8, input.length - i);"
                                   "var value = parseInt(input.substring(i, i + size), 10);"
                                   (str "if (size < 8) {"
                                        "var power = LuxRT$fromNumberI64(Math.pow(10, size));"
                                        "result = LuxRT$addI64(LuxRT$mulI64(result,power),LuxRT$fromNumberI64(value));"
                                        "}"
                                        "else {"
                                        "result = LuxRT$addI64(LuxRT$mulI64(result,chunkPower),LuxRT$fromNumberI64(value));"
                                        "}")
                                   "}")
                              "result = LuxRT$mulI64(result,LuxRT$fromNumberI64(sign));"
                              (str "return " (make-some "result") ";")
                              )
                         "}"
                         "else {"
                         (str "return " const-none ";")
                         "}")
                    "})")
   })

(def ^:private n64-methods
  {"divWord" (str "(function LuxRT$divWord(result, n, d) {"
                  "var dLong = LuxRT$makeI64(0,d);"
                  (str "if (LuxRT$eqI64(dLong,LuxRT$ONE)) {"
                       (str "result[0] = n.L;"
                            "result[1] = 0;"
                            "return")
                       "}"
                       "else {"
                       ;; Approximate the quotient and remainder
                       (str "var q = LuxRT$divI64(LuxRT$ushrI64(n,1),LuxRT$ushrI64(dLong,1));"
                            "var r = LuxRT$subI64(n,LuxRT$mulI64(q,dLong));"
                            ;; Correct the approximation
                            (str "while(LuxRT$ltI64(r,LuxRT$ZERO)) {"
                                 "r = LuxRT$addI64(r,dLong);"
                                 "q = LuxRT$subI64(q,LuxRT$ONE);"
                                 "}")
                            (str "while(LuxRT$ltI64(dLong,r) || LuxRT$eqI64(dLong,r)) {"
                                 "r = LuxRT$subI64(r,dLong);"
                                 "q = LuxRT$addI64(q,LuxRT$ONE);"
                                 "}")
                            "result[0] = q.L;"
                            "result[1] = r.L;"
                            )
                       "}")
                  "})")
   "primitiveShiftLeftBigInt" (str "(function LuxRT$primitiveShiftLeftBigInt(input,shift) {"
                                   "var output = input.slice();"
                                   "var shift2 = 32 - shift;"
                                   (str "for(var i = 0, c = output[i], m = (i + (input.length - 1)); i < m; i++) {"
                                        "var b = c;"
                                        "c = output[i+1];"
                                        "output[i] = (b << shift) | (c >>> shift2);"
                                        "}")
                                   "output[(input.length - 1)] <<= shift;"
                                   "return output;"
                                   "})")
   "primitiveShiftRightBigInt" (str "(function LuxRT$primitiveShiftRightBigInt(input,shift) {"
                                    "var output = input.slice();"
                                    "var shift2 = 32 - shift;"
                                    (str "for(var i = (input.length - 1), c = output[i]; i > 0; i--) {"
                                         "var b = c;"
                                         "c = output[i-1];"
                                         "output[i] = (c << shift2) | (b >>> shift);"
                                         "}")
                                    "output[0] >>>= shift;"
                                    "return output;"
                                    "})")
   "shiftLeftBigInt" (str "(function LuxRT$shiftLeftBigInt(input,shift) {"
                          "var shiftInts = shift >>> 5;"
                          "var shiftBits = shift & 0x1F;"
                          "var bitsInHighWord = LuxRT$countI64(LuxRT$makeI64(input[0],0));"
                          (str "if(shift <= (32 - bitsInHighWord)) {"
                               "var shifted = LuxRT$shlI64(LuxRT$makeI64(input[0],input[1]),shiftBits);"
                               "return [shifted.H,shifted.L];"
                               "}")
                          "var inputLen = input[0] === 0 ? 1 : 2;"
                          "var newLen = inputLen + shiftInts + 1;"
                          (str "if(shiftBits <= (32 - bitsInHighWord)) {"
                               "newLen--;"
                               "}")
                          (str "if(input.length < newLen) {"
                               ;; The array must grow
                               "input = [0|0,input[0],input[1]];"
                               "}")
                          (str "if(nBits == 0) {"
                               "return input;"
                               "}")
                          (str "if(shiftBits <= (32 - bitsInHighWord)) {"
                               "return LuxRT$primitiveShiftLeftBigInt(input,shiftBits);"
                               "}"
                               "else {"
                               "return LuxRT$primitiveShiftRightBigInt(input,(32 - shiftBits));"
                               "}")
                          "})")
   "shiftRightBigInt" (str "(function LuxRT$shiftRightBigInt(input,shift) {"
                           "var shiftInts = shift >>> 5;"
                           "var shiftBits = shift & 0x1F;"
                           "if(shiftBits === 0) { return input; }"
                           "var bitsInHighWord = LuxRT$countI64(LuxRT$makeI64(input[0],0));"
                           (str "if(shiftBits >= bitsInHighWord) {"
                                "return LuxRT$primitiveShiftLeftBigInt(input,(32-shiftBits));"
                                "}"
                                "else {"
                                "return LuxRT$primitiveShiftRightBigInt(input,shiftBits);"
                                "}")
                           "})")
   "mulsubBigInt" (str "(function LuxRT$mulsubBigInt(q, a, x, len, offset) {"
                       "var xLong = LuxRT$makeI64(0,x);"
                       "var carry = LuxRT$ZERO;"
                       "offset += len;"
                       (str "for (var j = len-1; j >= 0; j--) {"
                            "var product = LuxRT$addI64(LuxRT$mulI64(LuxRT$makeI64(0,a[j]),xLong),carry);"
                            "var difference = LuxRT$subI64(LuxRT$makeI64(0,q[offset]),product);"
                            "carry = LuxRT$addI64(LuxRT$ushrI64(product,32),((difference.L > ~product.L) ? LuxRT$ONE : LuxRT$ZERO));"
                            "}")
                       "return carry.L;"
                       "})")
   "divadd" (str "(function LuxRT$divadd(a, result, offset) {"
                 "var carry = LuxRT$ZERO;"
                 (str "for (var j = a.length - 1; j >= 0; j--) {"
                      "var sum = LuxRT$addI64(LuxRT$addI64(LuxRT$makeI64(0,a[j]),LuxRT$makeI64(0,result[j+offset])),carry);"
                      "result[j+offset] = sum.L;"
                      "carry = LuxRT$ushrI64(sum,32);"
                      "}")
                 "return carry.L;"
                 "})")
   "normalizeBigInt" (str "(function LuxRT$normalizeBigInt(input) {"
                          (str "if(input[0] !== 0) {"
                               "return LuxRT$makeI64(input[0],input[1]);"
                               "}"
                               "else {"
                               (str "var numZeros = 0;"
                                    (str "do {"
                                         "numZeros++;"
                                         "} while(numZeros < input.length && input[numZeros] == 0);")
                                    "var tempInput = input.slice(input.length-Math.max(2,input.length-numZeros));"
                                    "return LuxRT$makeI64(tempInput[0],tempInput[1]);")
                               "}")
                          "})")
   "divideOneWord" (str "(function LuxRT$divideOneWord(subject,param) {"
                        (str "var divLong = LuxRT$makeI64(0,param);"
                             ;; Special case of one word dividend
                             (str "if(subject.H === 0) {"
                                  (str "var remValue = LuxRT$makeI64(0,subject.L);"
                                       "var quotient = LuxRT$divI64(remValue,divLong);"
                                       "var remainder = LuxRT$subI64(remValue,LuxRT$mulI64(quotient.L,divLong));"
                                       "return [quotient,remainder];")
                                  "}")
                             "var quotient = [0|0,0|0];"
                             ;; Normalize the divisor
                             "var shift = 32 - LuxRT$countI64(LuxRT$makeI64(0,param));"
                             "var rem = subject.H;"
                             "var remLong = LuxRT$makeI64(0,rem);"
                             (str "if(LuxRT$ltI64(remLong,divLong)) {"
                                  "quotient[0] = 0|0;"
                                  "}"
                                  "else {"
                                  "quotient[0] = LuxRT$divI64(remLong,divLong).L;"
                                  "rem = LuxRT$subI64(remLong,LuxRT$mulI64(quotient[0],divLong)).L;"
                                  "remLong = LuxRT$makeI64(0,rem);"
                                  "}")
                             "var remBI = [subject.H,subject.L];"
                             "var xlen = 2;"
                             "var qWord = [0|0,0|0];"
                             (str "while(--xlen > 0) {"
                                  "var dividendEstimate = LuxRT$orI64(LuxRT$shlI64(remLong,32),LuxRT$makeI64(0,remBI[2 - xlen]));"
                                  (str "if(dividendEstimate >= 0) {"
                                       "var highWord = LuxRT$divI64(dividendEstimate,divLong);"
                                       "qWord[0] = highWord.L;"
                                       "qWord[1] = LuxRT$subI64(dividendEstimate,LuxRT$mulI64(highWord,divLong)).L;"
                                       "}"
                                       "else {"
                                       "LuxRT$divWord(qWord, dividendEstimate, param);"
                                       "}")
                                  "quotient[2 - xlen] = qWord[0];"
                                  "rem = qWord[1];"
                                  "remLong = LuxRT$makeI64(0,rem);"
                                  "}")
                             ;; Unnormalize
                             (str "if(shift > 0) {"
                                  "rem %= divisor;"
                                  "remBI[0] = rem;"
                                  "}"
                                  "else {"
                                  "remBI[0] = rem;"
                                  "}")
                             "var quotI64 = LuxRT$normalizeBigInt(quotient);"
                             "var remI64 = LuxRT$makeI64(remBI[0],remBI[1]);"
                             "return [quotI64,remI64];")
                        "})")
   "divmodBigInt" (str "(function LuxRT$divmodBigInt(subject,param) {"
                       (str "if(LuxRT$eqI64(param,LuxRT$ZERO)) {"
                            "throw new Error('Cannot divide by zero!');"
                            "}")
                       (str "if(LuxRT$eqI64(subject,LuxRT$ZERO)) {"
                            "return [LuxRT$ZERO, LuxRT$ZERO];"
                            "}")
                       (str "if(LuxRT$ltN64(subject,param)) {"
                            "return [LuxRT$ZERO, subject];"
                            "}")
                       (str "if(LuxRT$eqI64(subject,param)) {"
                            "return [LuxRT$ONE, LuxRT$ZERO];"
                            "}")
                       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                       (str "if (param.H === 0) {"
                            "return LuxRT$divideOneWord(subject,param.L);;"
                            "}")
                       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                       "var divisor = param;"
                       "var remainder = subject.H === 0 ? [0|0,subject.L] : [0|0,subject.H,subject.L];"
                       "var paramLength = param.H === 0 ? 1 : 2;"
                       "var subjLength = subject.H === 0 ? 1 : 2;"
                       "var limit = subjLength - paramLength + 1;"
                       "var quotient = (limit === 1) ? [0|0] : [0|0,0|0];"
                       ;; Normalize the divisor
                       "var shift = 32 - LuxRT$countI64(LuxRT$makeI64(divisor.H,0));"
                       (str "if(shift > 0) {"
                            "divisor = LuxRT$shlI64(divisor,shift);"
                            "remainder = LuxRT$shiftLeftBigInt(remainder,shift);"
                            "}")
                       (str "if((remainder.length-1) === subjLength) {"
                            "remainder[0] = 0;"
                            "}")
                       "var dh = divisor.H;"
                       "var dhLong = LuxRT$makeI64(0,dh);"
                       "var dl = divisor.L;"
                       "var qWord = [0|0,0|0];"
                       ;; D2 Initialize j
                       (str "for(var j = 0; j < limit; j++) {"
                            ;; D3 Calculate qhat
                            ;; estimate qhat
                            "var qhat = 0;"
                            "var qrem = 0;"
                            "var skipCorrection = false;"
                            "var nh = remainder[j];"
                            "var nh2 = nh + 0x80000000;"
                            "var nm = remainder[j+1];"
                            (str "if(nh == dh) {"
                                 (str "qhat = ~0;"
                                      "qrem = nh + nm;"
                                      "skipCorrection = (qrem + 0x80000000) < nh2;")
                                 "}"
                                 "else {"
                                 (str "var nChunk = LuxRT$orI64(LuxRT$shlI64(LuxRT$fromNumberI64(nh),32),LuxRT$fromNumberI64(nm));")
                                 (str "if(LuxRT$ltI64(LuxRT$ZERO,nChunk) || LuxRT$eqI64(LuxRT$ZERO,nChunk)) {"
                                      (str "qhat = LuxRT$divI64(nChunk,dhLong).L;"
                                           "qrem = LuxRT$subI64(nChunk,LuxRT$mulI64(qhat, dhLong)).L;")
                                      "}"
                                      "else {"
                                      (str "LuxRT$divWord(qWord, nChunk, dh);"
                                           "qhat = qWord[0];"
                                           "qrem = qWord[1];"
                                           )
                                      "}")
                                 "if(qhat == 0) { continue; }"
                                 (str "if(!skipCorrection) {"
                                      ;; Correct qhat
                                      (str "var qremLong = LuxRT$makeI64(0,qrem);"
                                           "var dlLong = LuxRT$makeI64(0,dl);"
                                           "var nl = LuxRT$makeI64(0,remainder[j+2]);"
                                           "var rs = LuxRT$orI64(LuxRT$shlI64(qremLong,32),nl);"
                                           "var estProduct = LuxRT$mulI64(dlLong,LuxRT$makeI64(0,qhat));"
                                           (str "if(LuxRT$ltN64(rs,estProduct)) {"
                                                (str "qhat--;"
                                                     "qrem = LuxRT$addI64(qremLong,dhLong).L;"
                                                     "qremLong = LuxRT$makeI64(0,qrem);"
                                                     (str "if(LuxRT$ltI64(dhLong,qremLong) || LuxRT$eqI64(dhLong,qremLong)) {"
                                                          (str "estProduct = LuxRT$mulI64(dlLong,LuxRT$makeI64(0,qhat));"
                                                               "rs = LuxRT$orI64(LuxRT$shlI64(qremLong,32),nl);"
                                                               "if(LuxRT$ltN64(rs,estProduct)) { qhat--; }")
                                                          "}"))
                                                "}")
                                           )
                                      "}")
                                 ;; D4 Multiply and subtract
                                 "remainder[j] = 0;"
                                 "var borrow = LuxRT$mulsubBigInt(remainder, divisor, qhat, paramLength, j);"
                                 ;; D5 Test remainder
                                 (str "if((borrow + 0x80000000) > nh2) {"
                                      ;; D6 Add back
                                      "LuxRT$divadd(divisor, remainder, j+1);"
                                      "qhat--;"
                                      "}")
                                 ;; Store the quotient digit
                                 "quotient[j] = qhat;"
                                 "}")
                            "}") ;; D7 loop on j
                       ;; D8 Unnormalize
                       "if(shift > 0) { remainder = LuxRT$shiftRightBigInt(remainder,shift); }"
                       "return [LuxRT$normalizeBigInt(quotient), LuxRT$normalizeBigInt(remainder)];"
                       "})")
   "encodeN64" (str "(function LuxRT$encodeN64(input) {"
                    (str "if(input.H < 0) {"
                         ;; Too big
                         "var lastDigit = LuxRT$remI64(input, LuxRT$makeI64(0,10));"
                         "var minusLastDigit = LuxRT$divI64(input, LuxRT$makeI64(0,10));"
                         "return '+'.concat(LuxRT$encodeI64(minusLastDigit)).concat(LuxRT$encodeI64(lastDigit));"
                         "}"
                         "else {"
                         ;; Small enough
                         "return '+'.concat(LuxRT$encodeI64(input));"
                         "}")
                    "})")
   "decodeN64" (str "(function LuxRT$decodeN64(input) {"
                    "input = LuxRT$clean_separators(input);"
                    (str "if(/^\\+\\d+$/.exec(input)) {"
                         (str "input = input.substring(1);")
                         (str "if(input.length <= 18) {"
                              ;; Short enough...
                              "return LuxRT$decodeI64(input);"
                              "}"
                              "else {"
                              ;; Too long
                              (str "var prefix = LuxRT$decodeI64(input.substring(0, input.length-1))[2];"
                                   "var suffix = LuxRT$decodeI64(input.charAt(input.length-1))[2];"
                                   "var total = LuxRT$addI64(LuxRT$mulI64(prefix,LuxRT$fromNumberI64(10)),suffix);"
                                   (str "if(LuxRT$ltN64(total,prefix)) {"
                                        (str "return " const-none ";")
                                        "}"
                                        "else {"
                                        (str "return " (make-some "total") ";")
                                        "}"))
                              "}")
                         "}"
                         "else {"
                         (str "return " const-none ";")
                         "}")
                    "})")
   "divN64" (str "(function LuxRT$divN64(l,r) {"
                 (str "if(LuxRT$ltI64(r,LuxRT$ZERO)) {"
                      (str "if(LuxRT$ltN64(l,r)) {"
                           "return LuxRT$ZERO;"
                           "}"
                           "else {"
                           "return LuxRT$ONE;"
                           "}")
                      "}"
                      "else if(LuxRT$ltI64(LuxRT$ZERO,l)) {"
                      "return LuxRT$divI64(l,r);"
                      "}"
                      "else {"
                      (str "if(LuxRT$eqI64(LuxRT$ZERO,r)) {"
                           "throw new Error('Cannot divide by zero!');"
                           "}"
                           "else {"
                           (str "if(LuxRT$ltI64(l,r)) {"
                                "return LuxRT$ZERO;"
                                "}"
                                "else {"
                                "return LuxRT$divmodBigInt(l,r)[0];"
                                "}")
                           "}")
                      "}")
                 "})")
   "remN64" (str "(function LuxRT$remN64(l,r) {"
                 (str "if(LuxRT$ltI64(l,LuxRT$ZERO) || LuxRT$ltI64(r,LuxRT$ZERO)) {"
                      (str "if(LuxRT$ltN64(l,r)) {"
                           "return l;"
                           "}"
                           "else {"
                           "return LuxRT$divmodBigInt(l,r)[1];"
                           "}")
                      "}"
                      "else {"
                      "return LuxRT$remI64(l,r);"
                      "}")
                 "})")
   "ltN64" (str "(function LuxRT$ltN64(l,r) {"
                "var li = LuxRT$addI64(l,LuxRT$MIN_VALUE_I64);"
                "var ri = LuxRT$addI64(r,LuxRT$MIN_VALUE_I64);"
                "return LuxRT$ltI64(li,ri);"
                "})")
   })

(def ^:private d64-methods
  {"mulD64" (str "(function LuxRT$mulD64(l,r) {"
                 "var lL = LuxRT$fromNumberI64(l.L);"
                 "var rL = LuxRT$fromNumberI64(r.L);"
                 "var lH = LuxRT$fromNumberI64(l.H);"
                 "var rH = LuxRT$fromNumberI64(r.H);"

                 "var bottom = LuxRT$ushrI64(LuxRT$mulI64(lL,rL),32);"
                 "var middle = LuxRT$addI64(LuxRT$mulI64(lH,rL),LuxRT$mulI64(lL,rH));"
                 "var top = LuxRT$mulI64(lH,rH);"

                 "var bottomAndMiddle = LuxRT$ushrI64(LuxRT$addI64(middle,bottom),32);"
                 
                 "return LuxRT$addI64(top,bottomAndMiddle);"
                 "})")
   "divD64" (str "(function LuxRT$divD64(l,r) {"
                 "return LuxRT$shlI64(LuxRT$divI64(l,LuxRT$fromNumberI64(r.H)),32);"
                 "})")
   "degToReal" (str "(function LuxRT$degToReal(input) {"
                    "var two32 = Math.pow(2,32);"
                    "var high = input.H / two32;"
                    "var low = (input.L / two32) / two32;"
                    "return high+low;"
                    "})")
   "realToDeg" (str "(function LuxRT$realToDeg(input) {"
                    "var two32 = Math.pow(2,32);"
                    "var shifted = (input % 1.0) * two32;"
                    "var low = ((shifted % 1.0) * two32) | 0;"
                    "var high = shifted | 0;"
                    "return LuxRT$makeI64(high,low);"
                    "})")
   "_add_deg_digit_powers" (str "(function LuxRT$_add_deg_digit_powers(left,right) {"
                                "var output = new Array(64);"
                                "var carry = 0;"
                                (str "for(var idx = 63; idx >= 0; idx--) {"
                                     "var raw = left[idx] + right[idx] + carry;"
                                     "output[idx] = raw % 10;"
                                     "raw = (raw / 10)|0;"
                                     "}")
                                "return output;"
                                "})")
   "_times5" (str "(function LuxRT$_times5(exp,digits) {"
                  "var carry = 0;"
                  (str "for(var idx = exp; idx >= 0; idx--) {"
                       "var raw = (digits[exp] * 5) + carry;"
                       "digits[exp] = raw % 10;"
                       "carry = (raw / 10)|0;"
                       "}")
                  "return digits;"
                  "})")
   "_deg_digit_power" (str "(function LuxRT$_deg_digit_power(exp) {"
                           "var digits = new Array(64);"
                           "digits[exp] = 1;"
                           (str "for(var idx = exp; idx >= 0; idx--) {"
                                "digits = LuxRT$_times5(exp,digits);"
                                "}")
                           "return digits;"
                           "})")
   "_bitIsSet" (str "(function LuxRT$_bitIsSet(input,idx) {"
                    "idx &= 63;"
                    (str "if(idx < 32) {"
                         "return (input.L & (1 << idx)) !== 0;"
                         "}")
                    (str "else {"
                         "return (input.H & (1 << (idx - 32))) !== 0;"
                         "}")
                    "})")
   "encodeD64" (str "(function LuxRT$encodeD64(input) {"
                    (str "if(LuxRT$eqI64(input,LuxRT$ZERO)) {"
                         "return '.0';"
                         "}")
                    "var digits = new Array(64);"
                    (str "for(var idx = 63; idx >= 0; idx--) {"
                         (str "if(LuxRT$_bitIsSet(input,idx)) {"
                              "var power = LuxRT$_deg_digit_power(63 - idx);"
                              "digits = LuxRT$_add_deg_digit_powers(digits,power);"
                              "}")
                         "}")
                    "var raw = '.'.concat(digits.join(''));"
                    "return raw.split(/0*$/)[0];"
                    "})")
   "deg_text_to_digits" (str "(function LuxRT$deg_text_to_digits(input) {"
                             "var output = new Array(64);"
                             (str "for(var idx = input.length-1; idx >= 0; idx--) {"
                                  "output[idx] = parseInt(input.substring(idx, idx+1));"
                                  "}")
                             "return output;"
                             "})")
   "deg_digits_lt" (str "(function LuxRT$deg_digits_lt(l,r) {"
                        (str "for(var idx = 0; idx < 64; idx++) {"
                             (str "if(l[idx] < r[idx]) {"
                                  "return true;"
                                  "}"
                                  "else if(l[idx] > r[idx]) {"
                                  "return false;"
                                  "}")
                             "}")
                        "return false;"
                        "})")
   "deg_digits_sub_once" (str "(function LuxRT$deg_digits_sub_once(target,digit,idx) {"
                              (str "while(true) {"
                                   (str "if(target[idx] > digit) {"
                                        (str "target[idx] = target[idx] - digit;"
                                             "return target;")
                                        "}"
                                        "else {"
                                        (str "target[idx] = 10 - (digit - target[idx]);"
                                             "idx--;"
                                             "digit=1;")
                                        "}")
                                   "}")
                              "})")
   "deg_digits_sub" (str "(function LuxRT$deg_digits_sub(l,r) {"
                         (str "for(var idx = 63; idx >= 0; idx--) {"
                              "l = LuxRT$deg_digits_sub_once(l,r[idx],idx);"
                              "}")
                         "return l;"
                         "})")
   "decodeD64" (let [failure (str "return " const-none ";")]
                 (str "(function LuxRT$decodeD64(input) {"
                      "input = LuxRT$clean_separators(input);"
                      (str "if(/^\\.\\d+$/.exec(input) && input.length <= 65) {"
                           (str "try {"
                                (str "var digits = LuxRT$deg_text_to_digits(input.substring(1));")
                                "var output = LuxRT$makeI64(0,0);"
                                (str "for(var idx = 0; idx < 64; idx++) {"
                                     "var power = LuxRT$deg_text_to_digits(idx);"
                                     (str "if(LuxRT$deg_digits_lt(power,digits)) {"
                                          (str "digits = LuxRT$deg_digits_sub(digits,power);"
                                               "var powerBit = LuxRT$shlI64(LuxRT$makeI64(0,1),(63-idx));"
                                               "output = LuxRT$orI64(output,powerBit);")
                                          "}")
                                     "}")
                                (str "return " (make-some "output") ";")
                                "}"
                                "catch(ex) {"
                                failure
                                "}")
                           "}"
                           "else {"
                           failure
                           "}")
                      "})"))
   })

(def ^:private io-methods
  {"log" (str "(function LuxRT$log(message) {"
              "console.log(message);"
              (str "return " &&/unit ";")
              "})")
   "error" (str "(function LuxRT$error(message) {"
                "throw new Error(message);"
                (str "return null;")
                "})")
   })

(def ^:private text-methods
  {"index" (str "(function LuxRT$index(text,part,start) {"
                "var idx = text.indexOf(part,LuxRT$toNumberI64(start));"
                (str (str "if(idx === -1) {"
                          "return " const-none ";"
                          "}")
                     (str "else {"
                          (str "return " (make-some "LuxRT$fromNumberI64(idx)") ";")
                          "}"))
                "})")
   "lastIndex" (str "(function LuxRT$lastIndex(text,part,start) {"
                    "var idx = text.lastIndexOf(part,LuxRT$toNumberI64(start));"
                    (str (str "if(idx === -1) {"
                              "return " const-none ";"
                              "}")
                         (str "else {"
                              (str "return " (make-some "LuxRT$fromNumberI64(idx)") ";")
                              "}"))
                    "})")
   "clip" (str "(function LuxRT$clip(text,from,to) {"
               (str "if(from.L > text.length || to.L > text.length) {"
                    (str "return " const-none ";")
                    "}"
                    "else {"
                    (str "return " (make-some "text.substring(from.L,to.L)") ";")
                    "}")
               "})")
   "replaceAll" (str "(function LuxRT$replaceAll(text,toFind,replaceWith) {"
                     "var reEscaped = toFind.replace(/[.*+?^${}()|[\\]\\\\]/g, '\\\\$&');"
                     "return text.replace(new RegExp(reEscaped, 'g'), replaceWith);"
                     "})")
   "textChar" (str "(function LuxRT$textChar(text,idx) {"
                   "var result = text.charAt(idx.L);"
                   (str "if(result === '') {"
                        (str "return " const-none ";")
                        "}"
                        "else {"
                        (str "return " (make-some "{'C':result}") ";")
                        "}")
                   "var reEscaped = toFind.replace(/[.*+?^${}()|[\\]\\\\]/g, '\\\\$&');"
                   "return text.replace(new RegExp(reEscaped, 'g'), replaceWith);"
                   "})")
   "textHash" (str "(function LuxRT$textHash(input) {"
                   "var hash = 0;"
                   (str "for(var i = 0; i < input.length; i++) {"
                        "hash = (((hash << 5) - hash) + input.charCodeAt(i)) & 0xFFFFFFFF;"
                        "}")
                   "return LuxRT$fromNumberI64(hash);"
                   "})")
   })

(def ^:private array-methods
  {"arrayGet" (str "(function LuxRT$arrayGet(arr,idx) {"
                   "var temp = arr[LuxRT$toNumberI64(idx)];"
                   (str "if(temp !== undefined) {"
                        (str "return " (make-some "temp") ";")
                        "}"
                        "else {"
                        (str "return " const-none ";")
                        "}")
                   "})")
   "arrayPut" (str "(function LuxRT$arrayPut(arr,idx,val) {"
                   "arr[LuxRT$toNumberI64(idx)] = val;"
                   "return arr;"
                   "})")
   "arrayRemove" (str "(function LuxRT$arrayRemove(arr,idx) {"
                      "delete arr[LuxRT$toNumberI64(idx)];"
                      "return arr;"
                      "})")
   })

(def ^:private bit-methods
  (let [make-basic-op (fn [op name]
                        (str "(function " name "(input,mask) {"
                             "return LuxRT$makeI64(input.H " op " mask.H, input.L " op " mask.L);"
                             "})"))]
    {"andI64" (make-basic-op "&" "LuxRT$andI64")
     "orI64" (make-basic-op "|" "LuxRT$orI64")
     "xorI64" (make-basic-op "^" "LuxRT$xorI64")
     "countI64" (str "(function LuxRT$countI64(input) {"
                     "var hs = (input.H).toString(2);"
                     "var ls = (input.L).toString(2);"
                     "var num1s = hs.concat(ls).replace(/0/g,'').length;"
                     "return LuxRT$fromNumberI64(num1s);"
                     "})")
     "shlI64" (str "(function LuxRT$shlI64(input,shift) {"
                   "shift &= 63;"
                   (str "if(shift === 0) {"
                        "return input;"
                        "}"
                        "else {"
                        (str "if (shift < 32) {"
                             "var high = (input.H << shift) | (input.L >>> (32 - shift));"
                             "var low = input.L << shift;"
                             "return LuxRT$makeI64(high, low);"
                             "}"
                             "else {"
                             "var high = (input.L << (shift - 32));"
                             "return LuxRT$makeI64(high, 0);"
                             "}")
                        "}")
                   "})")
     "shrI64" (str "(function LuxRT$shrI64(input,shift) {"
                   "shift &= 63;"
                   (str "if(shift === 0) {"
                        "return input;"
                        "}"
                        "else {"
                        (str "if (shift < 32) {"
                             "var high = input.H >> shift;"
                             "var low = (input.L >>> shift) | (input.H << (32 - shift));"
                             "return LuxRT$makeI64(high, low);"
                             "}"
                             "else {"
                             "var low = (input.H >> (shift - 32));"
                             "var high = input.H >= 0 ? 0 : -1;"
                             "return LuxRT$makeI64(high, low);"
                             "}")
                        "}")
                   "})")
     "ushrI64" (str "(function LuxRT$ushrI64(input,shift) {"
                    "shift &= 63;"
                    (str "if(shift === 0) {"
                         "return input;"
                         "}"
                         "else {"
                         (str "if (shift < 32) {"
                              "var high = input.H >>> shift;"
                              "var low = (input.L >>> shift) | (input.H << (32 - shift));"
                              "return LuxRT$makeI64(high, low);"
                              "}"
                              "else if(shift === 32) {"
                              "return LuxRT$makeI64(0, input.H);"
                              "}"
                              "else {"
                              "var low = (input.H >>> (shift - 32));"
                              "return LuxRT$makeI64(0, low);"
                              "}")
                         "}")
                    "})")
     }))

(def ^:private lux-methods
  {"clean_separators" (str "(function LuxRT$clean_separators(input) {"
                           "return input.replace(/_/g,'');"
                           "})")
   "runTry" (str "(function LuxRT$runTry(op) {"
                 (str "try {"
                      (str "return [1,'',op(null)];")
                      "}"
                      "catch(ex) {"
                      (str "return [0,null,ex.toString()];")
                      "}")
                 "})")
   "programArgs" (str "(function LuxRT$programArgs() {"
                      (str "if(typeof process !== 'undefined' && process.argv) {"
                           (str (str "var result = " const-none ";")
                                "for(var idx = process.argv.length-1; idx >= 0; idx--) {"
                                (str "result = " (make-some "[process.argv[idx],result]") ";")
                                "}")
                           (str "return result;")
                           "}"
                           "else {"
                           (str "return " const-none ";")
                           "}")
                      "})")
   })

(def ^:private js-methods
  {"jsSetField" (str "(function LuxRT$jsSetField(object, field, input) {"
                     "object[field] = input;"
                     "return object;"
                     "})")
   "jsDeleteField" (str "(function LuxRT$jsDeleteField(object, field) {"
                        "delete object[field];"
                        "return object;"
                        "})")
   "jsObjectCall" (str "(function LuxRT$jsObjectCall(object, method, args) {"
                       "return object[method].apply(object, args);"
                       "})")
   })

(def LuxRT "LuxRT")

(def compile-LuxRT
  (&&/save-js! LuxRT
               (->> (merge lux-methods
                           adt-methods
                           i64-methods
                           n64-methods
                           d64-methods
                           text-methods
                           array-methods
                           bit-methods
                           io-methods
                           js-methods)
                    (reduce (fn [prev [key val]] (str prev "var LuxRT$" key " = " val ";\n"))
                            ""))))
