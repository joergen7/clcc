[![Build Status](https://app.travis-ci.com/joergen7/clcc.svg?branch=master)](https://app.travis-ci.com/joergen7/clcc)

# clcc
C++ language model and compiler in Common Lisp

This library provides a model of the C++ programming language suitable as a target for compilation. Its goal is to both cover a large portion of C++ language features and be readable.


## System Requirements

- A Common Lisp distribution; I tested the following distributions:
  * [SBCL](https://www.sbcl.org/)
  * [ABCL](https://abcl.org/)
- [ASDF](https://asdf.common-lisp.dev/) comes packaged with the aforementioned CL distributions
- [lang-util](https://github.com/joergen7/lang-util/)
- [Alexandria](https://alexandria.common-lisp.dev/)
- [FiveAM](https://fiveam.common-lisp.dev/) and its dependencies

ASDF looks for system definitions in the `~/common-lisp/` folder. Create it and change to it.

``` bash
mkdir -p ~/common-lisp
cd ~/common-lisp
```

### Installing Alexandria

``` bash
git clone http://common-lisp.net/projects/alexandria/alexandria.git

```

### Installing FiveAM

To run the test suite you must have the [FiveAM](https://fiveam.common-lisp.dev/) regression testing framework and its dependencies available. We use ASDF to manage systems and their dependencies. In the `~/common-lisp` folder run

``` bash
git clone https://gitlab.common-lisp.net/trivial-backtrace/trivial-backtrace.git/
git clone https://github.com/didierverna/asdf-flv.git
git clone https://github.com/lispci/fiveam.git
```

### Adding clcc and lang-util

The clcc and lang-util libraries also need to be visible to ASDF. Assuming your clone of the lang-util repository resides in `~/git/lang-util/` and clcc resides in `~/git/clcc/` this can be accomplished by creating a symbolic link.

``` bash
ln -s ~/git/lang-util ~/common-lisp/lang-util
ln -s ~/git/clcc ~/common-lisp/clcc
```

Eventually, you should end up with a directory structure like this:

    ~/
	+- common-lisp/
	   +- alexandria/
	   +- asdf-flv/
	   +- clcc/
	   +- fiveam/
	   +- lang-util/
	   +- trivial-backtrace/

## Loading lang-util

You can load the library using either ASDF or Quicklisp. Below, we give instructions for each method.

### Loading with ASDF

Most Common Lisp distributions come with ASDF packaged. Thus, you can simply require ASDF and load the lang-util system like so:

``` cl
(require :asdf)
(asdf:load-system :clcc)
```

## Loading lang-util with Quicklisp

In addition to ASDF, you can load lang-util using [Quicklisp](https://www.quicklisp.org/beta/). Assuming, you have Quicklisp loaded you can run

``` cl
(ql:quickload :clcc)
```

## Testing

### Running the Test Suite

With FiveAM and its dependencies in place you can now run:

``` cl
(require :asdf)
(asdf:test-system :clcc)
```

### Coverage Info

Assuming, that you are running [SBCL](https://www.sbcl.org/) you can get coverage information using SBCL's  [sb-cover](http://www.sbcl.org/manual/#sb_002dcover) module.

``` cl
(require :asdf)
(require :sb-cover)
(declaim (optimize sb-cover:store-coverage-data))

(asdf:test-system :clcc :force t)
(sb-cover:report "coverage/")
```

## Examples

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)
