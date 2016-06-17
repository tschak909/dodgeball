dodgeball
=========

Dodgeball is a game for the Atari VCS (aka the Atari 2600).

It is a 2K cart, done in homage to the original launch titles, in that the graphics are simple, there will be multiple variations around the core game, and the focus is on solid two player gameplay.
 
It will require the joystick controllers.
 
Basic rules:
 
* Two players, in a playfield, with or without obstacles.
* Players can move freely around field and obstacles
* There is a computer controlled ball (using BL object) which will bounce around the playfield freely using PONG™ physics.
* Each player also has at their disposal, a ball which can be thrown by pressing trigger (using M0 and M1).
* Ball will automatically retract to player after user settable decay time (short, medium, long)
* if Computer ball hits a player, point is subtracted (unless 0).
* if P0's ball hits P1, P0 gets +1 points, P1 gets -1 (unless 0), and vice-versa.
* if P1 presses trigger at just the right moment when the ball is colliding with player, the above point rule flips, vice versa with other player.
* in both cases, ball is returned to the player, to be used immediately.
* Game ends after timer lapse (the classic 2 minutes 16 timer lapse)
* Game score blinks when timer is about to expire
* Game will indicate ball capture with color cycling display and sound effects
* Each ball bounce will be an appropriate ping.
 
The idea is to have game variations, which will vary the following:
 
* Playfield shape/obstacles
* Player ball retract time
* differences in player movement physics (dodgeball on ice?) :)
* difficulty switches will set _something_ ... either player size, or the addition of holes on the horizontal ends of field so that ball can wrap around?
