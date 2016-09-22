function fish_prompt
	test $SSH_TTY; and printf (set_color red)(whoami)(set_color white)'@'(set_color yellow)(hostname)' '

    test $USER = 'root'; and echo (set_color red)"#"

	if not set -q __fish_prompt_hostname
		set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
    end

    # Main
	
	echo -n $normal (set_color brgreen)"$USER"(set_color green)"@"(set_color brgreen)"$__fish_prompt_hostname"(set_color green)":"(set_color yellow)(prompt_pwd) (__fish_git_prompt) (set_color red)'❯'(set_color yellow)'❯'(set_color green)'❯ '
end
