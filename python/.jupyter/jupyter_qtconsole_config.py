# Configuration file for jupyter-qtconsole.

#------------------------------------------------------------------------------
# Configurable configuration
#------------------------------------------------------------------------------

import pkg_resources

#------------------------------------------------------------------------------
# LoggingConfigurable configuration
#------------------------------------------------------------------------------

# A parent class for Configurables that log.
#
# Subclasses have a log trait, and the default behavior is to get the logger
# from the currently running Application.

#------------------------------------------------------------------------------
# ConnectionFileMixin configuration
#------------------------------------------------------------------------------

# Mixin for configurable classes that work with connection files

# set the stdin (ROUTER) port [default: random]
# c.ConnectionFileMixin.stdin_port = 0

# Set the kernel's IP address [default localhost]. If the IP address is
# something other than localhost, then Consoles on other machines will be able
# to connect to the Kernel, so be careful!
# c.ConnectionFileMixin.ip = ''

# JSON file in which to store connection info [default: kernel-<pid>.json]
#
# This file will contain the IP, ports, and authentication key needed to connect
# clients to this kernel. By default, this file will be created in the security
# dir of the current profile, but can be specified by absolute path.
# c.ConnectionFileMixin.connection_file = ''

# set the shell (ROUTER) port [default: random]
# c.ConnectionFileMixin.shell_port = 0

# set the iopub (PUB) port [default: random]
# c.ConnectionFileMixin.iopub_port = 0

# set the heartbeat port [default: random]
# c.ConnectionFileMixin.hb_port = 0

# set the control (ROUTER) port [default: random]
# c.ConnectionFileMixin.control_port = 0

#
# c.ConnectionFileMixin.transport = 'tcp'

#------------------------------------------------------------------------------
# JupyterConsoleApp configuration
#------------------------------------------------------------------------------

# Set to display confirmation dialog on exit. You can always use 'exit' or
# 'quit', to force a direct exit without any confirmation.
# c.JupyterConsoleApp.confirm_exit = True

# Connect to an already running kernel
# c.JupyterConsoleApp.existing = ''

# The name of the default kernel to start.
# c.JupyterConsoleApp.kernel_name = 'python'

# The SSH server to use to connect to the kernel.
# c.JupyterConsoleApp.sshserver = ''

# Path to the ssh key to use for logging in to the ssh server.
# c.JupyterConsoleApp.sshkey = ''

#------------------------------------------------------------------------------
# SingletonConfigurable configuration
#------------------------------------------------------------------------------

# A configurable that only allows one instance.
#
# This class is for classes that should only have one instance of itself or
# *any* subclass. To create and retrieve such a class use the
# :meth:`SingletonConfigurable.instance` method.

#------------------------------------------------------------------------------
# Application configuration
#------------------------------------------------------------------------------

# This is an application.

# Set the log level by value or name.
# c.Application.log_level = 30

# The Logging format template
# c.Application.log_format = '[%(name)s]%(highlevel)s %(message)s'

# The date format used by logging formatters for %(asctime)s
# c.Application.log_datefmt = '%Y-%m-%d %H:%M:%S'

#------------------------------------------------------------------------------
# JupyterApp configuration
#------------------------------------------------------------------------------

# Base class for Jupyter applications

# Full path of a config file.
# c.JupyterApp.config_file = ''

# Generate default config file.
# c.JupyterApp.generate_config = False

# Specify a config file to load.
# c.JupyterApp.config_file_name = ''

# Answer yes to any prompts.
# c.JupyterApp.answer_yes = False

#------------------------------------------------------------------------------
# JupyterQtConsoleApp configuration
#------------------------------------------------------------------------------

# Start the console window maximized.
# c.JupyterQtConsoleApp.maximize = False

# Use a plaintext widget instead of rich text (plain can't print/save).
# c.JupyterQtConsoleApp.plain = False

# Whether to display a banner upon starting the QtConsole.
c.JupyterQtConsoleApp.display_banner = False

# Start the console window with the menu bar hidden.
# c.JupyterQtConsoleApp.hide_menubar = False

# path to a custom CSS stylesheet
c.JupyterQtConsoleApp.stylesheet = pkg_resources.resource_filename(
    'jupyter_qtconsole_colorschemes', 'zenburn.css')

#------------------------------------------------------------------------------
# NewBase configuration
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# ConsoleWidget configuration
#------------------------------------------------------------------------------

# An abstract base class for console-type widgets. This class has functionality
# for:
#
#     * Maintaining a prompt and editing region
#     * Providing the traditional Unix-style console keyboard shortcuts
#     * Performing tab completion
#     * Paging text
#     * Handling ANSI escape codes
#
# ConsoleWidget also provides a number of utility methods that will be
# convenient to implementors of a console-style widget.

# The font size. If unconfigured, Qt will be entrusted with the size of the
# font.
c.ConsoleWidget.font_size = 13

# The type of underlying text widget to use. Valid values are 'plain', which
# specifies a QPlainTextEdit, and 'rich', which specifies a QTextEdit.
# c.ConsoleWidget.kind = 'plain'

# The width of the console at start time in number of characters (will double
# with `hsplit` paging)
# c.ConsoleWidget.width = 81

# The maximum number of lines of text before truncation. Specifying a non-
# positive number disables text truncation (not recommended).
c.ConsoleWidget.buffer_size = 10000

# The type of completer to use. Valid values are:
#
# 'plain'   : Show the available completion as a text list
#             Below the editing area.
# 'droplist': Show the completion in a drop down list navigable
#             by the arrow keys, and from which you can select
#             completion by pressing Return.
# 'ncurses' : Show the completion as a text list which is navigable by
#             `tab` and arrow keys.
# c.ConsoleWidget.gui_completion = 'ncurses'

# The height of the console at start time in number of characters (will double
# with `vsplit` paging)
c.ConsoleWidget.height = 40

# The font family to use for the console. On OSX this defaults to Monaco, on
# Windows the default is Consolas with fallback of Courier, and on other
# platforms the default is Monospace.
c.ConsoleWidget.font_family = 'Source Code Pro'

# Whether to process ANSI escape codes.
# c.ConsoleWidget.ansi_codes = True

# The type of paging to use. Valid values are:
#
# 'inside'
#    The widget pages like a traditional terminal.
# 'hsplit'
#    When paging is requested, the widget is split horizontally. The top
#    pane contains the console, and the bottom pane contains the paged text.
# 'vsplit'
#    Similar to 'hsplit', except that a vertical splitter is used.
# 'custom'
#    No action is taken by the widget beyond emitting a
#    'custom_page_requested(str)' signal.
# 'none'
#    The text is written directly to the console.
c.ConsoleWidget.paging = 'hsplit'

# Whether to automatically execute on syntactically complete input.
#
# If False, Shift-Enter is required to submit each execution. Disabling this is
# mainly useful for non-Python kernels, where the completion check would be
# wrong.
# c.ConsoleWidget.execute_on_complete_input = True

# Whether to include output from clients other than this one sharing the same
# kernel.
#
# Outputs are not displayed until enter is pressed.
# c.ConsoleWidget.include_other_output = False

#------------------------------------------------------------------------------
# HistoryConsoleWidget configuration
#------------------------------------------------------------------------------

# A ConsoleWidget that keeps a history of the commands that have been executed
# and provides a readline-esque interface to this history.

#
# c.HistoryConsoleWidget.history_lock = False

#------------------------------------------------------------------------------
# FrontendWidget configuration
#------------------------------------------------------------------------------

# A Qt frontend for a generic Python kernel.

# Whether to draw information calltips on open-parentheses.
# c.FrontendWidget.enable_calltips = True

# Seconds to wait for is_complete replies from the kernel.
# c.FrontendWidget.is_complete_timeout = 0.25

# Whether to clear the console when the kernel is restarted
# c.FrontendWidget.clear_on_kernel_restart = True

# Whether to ask for user confirmation when restarting kernel
# c.FrontendWidget.confirm_restart = True

# The pygments lexer class to use.
# c.FrontendWidget.lexer_class = traitlets.Undefined

#
# c.FrontendWidget.banner = ''

#------------------------------------------------------------------------------
# IPythonWidget configuration
#------------------------------------------------------------------------------

# Dummy class for config inheritance. Destroyed below.

#------------------------------------------------------------------------------
# JupyterWidget configuration
#------------------------------------------------------------------------------

# A FrontendWidget for a Jupyter kernel.

# A command for invoking a system text editor. If the string contains a
# {filename} format specifier, it will be used. Otherwise, the filename will be
# appended to the end the command.
# c.JupyterWidget.editor = ''

# If not empty, use this Pygments style for syntax highlighting. Otherwise, the
# style sheet is queried for Pygments style information.
c.JupyterWidget.syntax_style = 'zenburn'

#
# c.JupyterWidget.in_prompt = 'In [<span class="in-prompt-number">%i</span>]: '

#
# c.JupyterWidget.out_prompt = 'Out[<span class="out-prompt-number">%i</span>]: '

#
# c.JupyterWidget.output_sep2 = ''

# The editor command to use when a specific line number is requested. The string
# should contain two format specifiers: {line} and {filename}. If this parameter
# is not specified, the line number option to the %edit magic will be ignored.
# c.JupyterWidget.editor_line = ''

# A CSS stylesheet. The stylesheet can contain classes for:
#     1. Qt: QPlainTextEdit, QFrame, QWidget, etc
#     2. Pygments: .c, .k, .o, etc. (see PygmentsHighlighter)
#     3. QtConsole: .error, .in-prompt, .out-prompt, etc
# c.JupyterWidget.style_sheet = ''

#
# c.JupyterWidget.output_sep = ''

#
# c.JupyterWidget.input_sep = '\n'

#------------------------------------------------------------------------------
# KernelManager configuration
#------------------------------------------------------------------------------

# Manages a single kernel in a subprocess on this host.
#
# This version starts kernels with Popen.

# DEPRECATED: Use kernel_name instead.
#
# The Popen Command to launch the kernel. Override this if you have a custom
# kernel. If kernel_cmd is specified in a configuration file, Jupyter does not
# pass any arguments to the kernel, because it cannot make any assumptions about
# the arguments that the kernel understands. In particular, this means that the
# kernel does not receive the option --debug if it given on the Jupyter command
# line.
# c.KernelManager.kernel_cmd = traitlets.Undefined

# Should we autorestart the kernel if it dies.
# c.KernelManager.autorestart = False

#------------------------------------------------------------------------------
# Session configuration
#------------------------------------------------------------------------------

# Object for handling serialization and sending of messages.
#
# The Session object handles building messages and sending them with ZMQ sockets
# or ZMQStream objects.  Objects can communicate with each other over the
# network via Session objects, and only need to work with the dict-based IPython
# message spec. The Session will handle serialization/deserialization, security,
# and metadata.
#
# Sessions support configurable serialization via packer/unpacker traits, and
# signing with HMAC digests via the key/keyfile traits.
#
# Parameters ----------
#
# debug : bool
#     whether to trigger extra debugging statements
# packer/unpacker : str : 'json', 'pickle' or import_string
#     importstrings for methods to serialize message parts.  If just
#     'json' or 'pickle', predefined JSON and pickle packers will be used.
#     Otherwise, the entire importstring must be used.
#
#     The functions must accept at least valid JSON input, and output *bytes*.
#
#     For example, to use msgpack:
#     packer = 'msgpack.packb', unpacker='msgpack.unpackb'
# pack/unpack : callables
#     You can also set the pack/unpack callables for serialization directly.
# session : bytes
#     the ID of this Session object.  The default is to generate a new UUID.
# username : unicode
#     username added to message headers.  The default is to ask the OS.
# key : bytes
#     The key used to initialize an HMAC signature.  If unset, messages
#     will not be signed or checked.
# keyfile : filepath
#     The file containing a key.  If this is set, `key` will be initialized
#     to the contents of the file.

# The UUID identifying this session.
# c.Session.session = ''

# Metadata dictionary, which serves as the default top-level metadata dict for
# each message.
# c.Session.metadata = traitlets.Undefined

# Threshold (in bytes) beyond which a buffer should be sent without copying.
# c.Session.copy_threshold = 65536

# Username for the Session. Default is your system username.
# c.Session.username = 'swiesner'

# path to file containing execution key.
# c.Session.keyfile = ''

# Debug output in the Session
# c.Session.debug = False

# The name of the packer for serializing messages. Should be one of 'json',
# 'pickle', or an import name for a custom callable serializer.
# c.Session.packer = 'json'

# The maximum number of digests to remember.
#
# The digest history will be culled when it exceeds this value.
# c.Session.digest_history_size = 65536

# The digest scheme used to construct the message signatures. Must have the form
# 'hmac-HASH'.
# c.Session.signature_scheme = 'hmac-sha256'

# The maximum number of items for a container to be introspected for custom
# serialization. Containers larger than this are pickled outright.
# c.Session.item_threshold = 64

# The name of the unpacker for unserializing messages. Only used with custom
# functions for `packer`.
# c.Session.unpacker = 'json'

# execution key, for signing messages.
# c.Session.key = b''

# Threshold (in bytes) beyond which an object's buffer should be extracted to
# avoid pickling.
# c.Session.buffer_threshold = 1024
