setup: setup_dotfiles

setup_dotfiles: setup_shell setup_git setup_emacs setup_python setup_terminator

setup_shell:
	stow shell --target=${HOME}

setup_git:
	stow git --target=${HOME}

setup_emacs:
	stow emacs --target=${HOME}

setup_python:
	stow python --target=${HOME}

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

ubuntu_config:
	make setup_typeahead
	make disable_screenshot_sound
	make setup_syncthing

setup_syncthing:
	systemctl --user enable syncthing.service
	systemctl --user start syncthing.service

setup_typeahead:
	sudo add-apt-repository ppa:lubomir-brindza/nautilus-typeahead
	sudo apt install nautilus

disable_screenshot_sound:
	sudo mv /usr/share/sounds/freedesktop/stereo/screen-capture.oga /usr/share/sounds/freedesktop/stereo/screen-capture.oga.bak
