## Lux Mode

An Emacs mode for *the Lux programming language*.

You can create a directory inside your `.emacs.d` directory named `el_files`.

Then, you can add this to your *init.el* file:

	(add-to-list 'load-path "~/.emacs.d/el_files/")
	(require 'lux-mode)

If you use Paredit or Rainbow-parens, you can hook them up to Lux mode:

	(add-hook 'lux-mode-hook #'paredit-mode)
	(add-hook 'lux-mode-hook #'rainbow-delimiters-mode)

