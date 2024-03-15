setup: setup_dotfiles

setup_dotfiles: setup_shell setup_git setup_emacs setup_terminator

setup_shell:
	stow shell --target=${HOME}

setup_git:
	stow git --target=${HOME}

setup_emacs:
	stow emacs --target=${HOME}

setup_terminator:
	stow terminator --target=${HOME}


basic_ubuntu: package_install_apt_core package_install_apt_media install_starship

install_packages:
	@if command -v apt >/dev/null 2>&1; then \
	    make package_install_apt_core; \
	fi

package_install_apt_core:
	sudo apt update && xargs -a packages/packages-apt-core sudo apt install -y

package_install_apt_media:
	sudo apt update && xargs -a packages/packages-apt-media sudo apt install -y

install_starship:
        # starship configured under shell
	curl -sS https://starship.rs/install.sh | sh


