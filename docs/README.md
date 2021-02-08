# Docs of Kai

## Setting the Default Browser

Often the browser settings used for the Internet are not ideal for
viewing plots.  You can control how plots are displayed in the browser
by setting rendering options in the file `kai-init.lisp`.

By default, kai will render the in the system default browser and it
will be displayed just any other web page would be.

To use a browser other than the default, or to pass additional options
to the renderer when plotting, you can configure the options as in the
example below.

Chrome seems to work best when rendering plots. To configure another
browser, you will need to figure out the command line switches for
that browser, and then configure `*default-browser-options*`
appropriately.

The special variable `*browser-commands*` contains the path the
browser executables, conditionalised by OS. To set your default to
Chrome, place this form in kai-init.lisp:

```lisp
(defparameter *default-browser-command* :chrome)
```

If you do not set one, the default system browser will be used. Chrome
and system defaults are the only ones implemented as of now. To add
additional browsers, perhaps Electron, add it to the
`*browser-commands*` alist in `src/browser.lisp`.

## Command-line switch options to the browser

Browsers differ widely in their processing of command line
arguments. So much so that any configuration by command line is best
avoided in favour of JavaScript. As an example, setting window size
via a command line is not supported on all browsers, and for those
that it does it requires other switches to be set, and all vary
between browsers. JavaScript on the other hand works identically
across browsers, so it is best to inject this kind of code into the
page before it is rendered.

There are some configurations however, that must be done from the
command line. Chrome's 'app mode' is an example of this. For
configuration that must be done at the command line, the functions in
`src/browser.lisp` can be used to assemble a command line. See the
Chrome configuration for an example.

## Chrome Options

Kai comes with a commonly used set of options already set for Chrome:

```lisp
(defparameter *default-chrome-options*
  (list (cons "window-size" "800,600")
	    (cons "app" "foo")))
```

If you want to use these, set them to the defaults:

```lisp
(defparameter *default-browser-options* *default-chrome-options*)
```

Manual configuration of these is described below.

### Running in App mode

--app=
: Start Chrome in application mode, with no menus, tabs or other
  distractions. This is a good option for displaying plots.

To set this:
```lisp
(push (cons "window-size" "800,600") *default-browser-options*)
```

### Setting the window size

Many people run large sized browser windows when viewing Internet
material. This isn't ideal for viewing plots. To change the plotting
window size, you can set the following:

```lisp
(push (cons "app" "foo") *default-browser-options*)
```

Note here that we're pushing directly onto
`*default-browser-options*`.

To change the size of the plot interactively from the REPL, use the
`set-chrome-size` function. Here's an example that changes the plot size
from the example above, 800x600, to 640x480:

```lisp
(set-chrome-size "1024,768")
```

### Setting browser FAQ

#### I'm running on a platform other than Windows

If you want to use a browser on another platform, edit the
`browser.lisp` and add the conditionalised path to the browser
command. E.g. adding Linux Chrome:

```lisp
(cons :chrome #+windows "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe"
		      #+macos "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
			  #+linux ...
```

If you want to use a different browser, add another keyword:

```lisp
(cons :safari "/Applications/Safari.app/Contents/MacOS/safari") ; may not be correct
```
