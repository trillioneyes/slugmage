Cannibal Slugmage of Eden is an old Ludum Dare entry that I never finished.

The theme was evolution. You play as a cannibal slugmage, trying to breed a naturally-evolving population of slugs to better fuel your magic and eventually allow you to remake the world in your own image. The competition version was nowhere near as cool as it sounds, but I hope to turn Slugmage into the game it was meant to be!


### Build Instructions
There is currently only a build script for SBCL. I hope to write a more portable script soon.

  1. Make sure you have quicklisp and lispbuilder-sdl.
  2. If you're on Windows, make sure `SDL.dll` is in the source directory.
  3. Run `sbcl --load make.lisp`. If you want, you can also specify an alternate name for the output (the default is `slugmage.exe`, because I started the project on windows): `sbcl --load make.lisp slugmage`.

And now you should be able to run it! (If you move the executable, make sure the images are in the same directory, and on windows that you also have SDL.dll.)