# Configuration file for ipython.

print("\n>>> Using custom ipython ipython_config.py file with autoreload :)\n")

c = get_config()  # noqa

# Load the autoreload extension
c.InteractiveShellApp.extensions = ['autoreload']

# Set autoreload mode to '2' so all modules are reloaded before executing code
c.InteractiveShellApp.exec_lines = ['%autoreload 2']
