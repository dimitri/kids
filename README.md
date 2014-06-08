# Kids discovery of computing

Having a computing background nowadays isn't any more luxury, it's going to
be the bare minimum kit of survival in the next generation's jungle. So how
do we prepare kids for that?

## The setup

It all happens with a Raspberry Pi, and begins in linux console mode.

The idea behind this whole experience is to teach (my) kids that they
actually do own the computer, which is only going to do whatever they tell
it to, and nothing else. I think that the easiest (and best) way to have
them understand that is to confront them with the console linux (another
free unix would do) prompt.

At the linux prompt, nothing happens short of you typing a command.

The next step is writting the games or applications that we're going to use
together.

The step after that is to write an educational game where the movements are
not controled anymore by direct actions on the keyboard (arrows, etc) but
rather by evaluating code. Same kind of games, same kind of goals, just with
programming as the tool to achieve the goals.

## First games

Very simple approach to how the console works:

  - `sl` and variants `sl -F` and `sl -al` are a big hit
  - `cat` is loads of fun
  - `echo` less so
  - `bc` is fun for a very little while
  - `figlet` is very fun
  - `cat|figlet` is quite a hit

It might not seem like a lot, but it covered like the first 5 sessions at
the computer, if not more. The kids are still getting back there for
amusement.

You wouldn't believe how much fun `cat` is for a kid currently learning how
to read and write at school... well I know I didn't expect that much fun out
of that little `cat` program.

## On to BSD Games

So much fun ahead!

  - `snake` is like wow!
  - `worm` barely can keep up the amazement, but actually does
  - `hangman` is a hit too, as soon as daddy finds
  - `alias pendu='hangman -d /usr/share/dict/french'`

## What about writing our own, kids?

We've been using the `clisp` Common Lisp implementation for that, because it
comes with support for keyboard events and console based window display
without any third party libs.

The first game is more about daddy getting used to the API than a real game,
you might recognise a strange LOGO feeling to it with a *pencil* that you
can toggle using, and a special *eraser* to redraw some parts.

See the file `pencil.lisp`, that's been written while the kids were waiting
for some results, under their directions, with much frustration each time a
*bug* would be uncovered, only to get happy when daddy explains *oh it's
much easier to write bugs than software apparently, let me try and fix this
one*.

### Running it

It's actually quite simple:

    $ apt-get install clisp
    $ clisp -i pencil.lisp
    
Then I would advice something in the spirit of:

    $ alias game=`clisp -i pencil.lisp`
    
### The history / background / comments

Also, writing the code on the *Raspberry Pi* console with a wrong keyboard
map (come on it's a plain qwerty keyboard here...) and with a bare Emacs
setup has not been helping much. Still, fun ;-)

The game has not been written the way you see it in one go, the steps sent
as following:

  - a first version taking control of the screen, filling it with `@`,
  - then, erasing behing the `@` as it progressed through the screen,
  - then, don't erase the borders of what's going to be a game board,
  - then, add in random elements, let's call that walls,
  - now, kid wanted to draw on the screen, add that capability,
  - oh, add an eraser, that's important,
  - please allow me to move the cursor while erasing.
  
You will note that the *width* and *height* notions in `pencil.lisp` are
between surprising and wrong, I started wrong headed and couldn't both keep
up with the new features and refactor. You might have heard about that story
before, right?

It might come to be a great lesson later when kids are beginning to read
code and will ask about those completely wrong things, then we will think
and fix them together, or maybe at this point they will just fix them a make
lots of fun about daddy coding abilities... some more fun ahead.

## Another one

Now school is all about summing numbers with two digits. Can we program a
training game for that very funny exercise?

    clisp -i ./sums.lisp

The game only accepts the correct digit, and you can quit at any time by
entering either `q` or `x`. After each successful computation you're
rewarded in french with a *Bravo* and a statement about the sum you just
did, just as in the following *screenshot*:

      Bravo!  82 + 53 = 135.
    
    
    
              8 2
    
            + 5 3
    
            ------
    
            1 3 5

Go play now!
