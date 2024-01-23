setup: setup_zsh setup_emacs setup_terminator

setup_zsh:
	stow zsh --target=${HOME}

setup_emacs:
	stow emacs --target=${HOME}

setup_terminator:
	stow terminator --target=${HOME}
