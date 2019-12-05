# Kai

Kai is a portable plotter for Common Lisp.

## Usage

```lisp
;; Prepare input data
(defparameter data '((3.1 4.0) (-10 2) (4.1 -7.8)))

;; Plot
(kai:plot data)
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
