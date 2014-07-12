Cannibal Slugmage of Eden is an old Ludum Dare entry that I never finished.

The theme was evolution. You play as a cannibal slugmage, trying to breed a naturally-evolving population of slugs to better fuel your magic and eventually allow you to remake the world in your own image. The competition version was nowhere near as cool as it sounds, but I hope to turn Slugmage into the game it was meant to be!


### Build Instructions
There is currently only a build script for SBCL. I hope to write a more portable and automatic script soon.

  1. Make sure you have quicklisp and lispbuilder-sdl.
  2. If you're on Windows, make sure `SDL.dll` is in the source directory.
  3. Run `sbcl` and use the following repl commands:

```
(compile-file "make.lisp")
(load "make.fasl")
(make)
```
   If you want, you can pass a string argument to `make` to set a different 
   executable name (like `(make "slugmagething")`). By default it's called
   `slugmage.exe`, because I started the project on Windows.

And now you should be able to run it! (Note that on Windows, if you want to move the executable, you'll need to make sure `SDL.dll` is in the same directory.)