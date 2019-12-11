# Kai

Kai is a portable plotter for Common Lisp.

![sample_sin] (./images/sample_sin.png)

## Usage

Prepare some input data.

```lisp
(defparameter data '((3.1 4.0) (-10 2) (4.1 -7.8)))
```

Then, show plotted figure with just one command.

```lisp
(kai:plot data)
```

You can add option.

```lisp
(kai:plot data
          :type :dot
          :color :red
          :title "test plot")
```


## Installation

### ASDF

Clone this repository:

```
$ git clone https://github.com/komi1230/kai
```

And load this system with ASDF:

```lisp
(asdf:load-system :kai)
```

### Quicklisp

Coming soon...
