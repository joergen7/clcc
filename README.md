# clcc
C++ language model and compiler in Common Lisp

The clcc library provides Common Lisp bindings to generate C++ code.

## System Requirements

- A Common Lisp distribution; I tested the following distributions:
  * [SBCL](https://www.sbcl.org/)
  * [ABCL](https://abcl.org/)
- lang-util
- [Alexandria](https://alexandria.common-lisp.dev/)
- [FiveAM](https://fiveam.common-lisp.dev/) and its dependencies

In the following, we give advice on how to set up your `common-lisp` directory. In order to create and switch to it enter

``` bash
mkdir -p ~/common-lisp
cd ~/common-lisp
```

### Installing Alexandria

``` bash
git clone http://common-lisp.net/projects/alexandria/alexandria.git

```

### Installing FiveAM

To run the test suite you must have the [FiveAM](https://fiveam.common-lisp.dev/) regression testing framework and its dependencies available. In the `common-lisp` folder relative to your home directory run

``` bash
git clone http://common-lisp.net/project/trivial-backtrace/trivial-backtrace.git
git clone https://github.com/didierverna/asdf-flv.git
git clone https://github.com/lispci/fiveam.git
```

### Adding clcc and lang-util

Checkout clcc and lang-util from GitHub

``` bash
git clone https://github.com/joergen7/lang-util.git
git clone https://github.com/joergen7/clcc.git
```

You can load clcc using [ASDF](https://asdf.common-lisp.dev/). ASDF looks for system definitions in the `common-lisp` folder relative to the home directory. Thus, the first step is to create a link here to the `lang-util` folder. Assuming your clone of clcc and lang-util reside in `~/git/clcc` and `~/git/lang-util` this can be accomplished by creating two symbolic links.

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
(asdf:test-system :clcc)
(sb-cover:report "coverage/")
```

## Examples

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)
