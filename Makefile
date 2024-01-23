setup: install_packages setup_dotfiles

install_packages:
	@if command -v apt >/dev/null 2>&1; then \
	    make package_install_apt_core; \
	fi

package_install_apt_core:
	sudo apt update && xargs -a packages/packages-apt-core sudo apt install -y

package_install_apt_media:
	sudo apt update && xargs -a packages/packages-apt-media sudo apt install -y

setup_dotfiles: setup_zsh setup_emacs setup_terminator

setup_zsh:
	stow zsh --target=${HOME}

setup_emacs:
	stow emacs --target=${HOME}

setup_terminator:
	stow terminator --target=${HOME}
