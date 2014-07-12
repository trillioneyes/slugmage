Cannibal Slugmage of Eden is an old Ludum Dare entry that I never finished.

The theme was evolution. You play as a cannibal slugmage, trying to breed a naturally-evolving population of slugs to better fuel your magic and eventually allow you to remake the world in your own image. The competition version was nowhere near as cool as it sounds, but I hope to turn Slugmage into the game it was meant to be!


### Build Instructions
There is currently only a build script for SBCL. I hope to write a more portable script soon.

  1. Make sure you have quicklisp. (lispbuilder-sdl is also required, but quicklisp does a pretty good job of installing it and its dependencies.)
  2. If you're on Windows, make sure `SDL.dll` is in the source directory.
  3. Run `sbcl --load make.lisp`. If you want, you can also specify an alternate name for the output (the default is `slugmage.exe`, because I started the project on windows): `sbcl --load make.lisp slugmage`.

And now you should be able to run it! (If you move the executable, make sure the images are in the same directory, and on windows that you also have SDL.dll.)

### Gameplay Instructions
The movement controls are somewhat nonintuitive right now and not yet rebindable: they are a (qwerty-based)
grid consisting of `qweasdzxc`. `s` (in the middle) is the wait key; the others move in the corresponding
direction (so `q` is up-left, `d` is right, etc).

You can pick up slugs by walking next to them and pressing `g`, then the appropriate direction. Slugs you pick
up will appear in your inventory. You can also press `t` and click a slug to drop it.

Alternately, once you have some slugs in your inventory, you can press `m` to do magic. Here, you can click 
on slugs in your inventory to eat them, adding their Mana score to your transitory total. When you have enough
mana to cast a spell (check their costs by mousing over them; they're the colored squares on the UI bar), you
can click the spell box and then finally click on a target in the world to cast it. Note that mana works a bit
like it does in Magic: The Gathering: all mana left over at the end of your turn is lost.

As for what the spells do: Lightning and Fire are currently the same (Fire used to kill an entire stack of
slugs, and still will if there is ever a stack, but slugs shouldn't be able to colocate anymore). Hand allows
you to grab a target slug from arbitrarily far away, and Dawn displays the victory message. (I would like
Dawn to eventually have a pretty animation, but currently there's nothing at all.)