;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2009 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : sf-module.lisp
;;; Version     : 8.0
;;; 
;;; Description : Module and device for connecting to the hacked synchronous python SF
;;;               game via a socket connection along with the code to run the model
;;;               and game in that way.
;;;
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;;
;;; 2009.03.26 Dan
;;;             : * Initial version with "nothing" in it...
;;; 2009.12.01 Dan [1.1]
;;;             : * Stable version that can only handle the orbit condition
;;;             :   for now since all it sees is the ship itself.
;;; 2009.12.04 Dan 
;;;             : * Changing the slack hook to just schedule update-world
;;;             :   everytime since that's safe to do now and that will cause
;;;             :   conflict-resolution to "move back" when necessary.
;;; 2010.01.25 Dan
;;;             : * Added the code to load the other modules used at the end
;;;             :   instead of assuming that they're in place when ACT-R itself
;;;             :   is loaded -- which means the "speculative" modules need to
;;;             :   be in the right place.
;;; 2010.01.28 Dan
;;;             : * Added a wait to the connection code to make sure we get the
;;;             :   initial time before starting and also adding a game update
;;;             :   right at the start of the run now.
;;;             : * In addition, changed the time at which the slack schedules
;;;             :   the update because otherwise we're behind a tick in perception
;;;             :   because of how the game handles things (our 'ready' signal
;;;             :   is an action it processes before sending us the data which
;;;             :   will be followed by the next stopping time).
;;; 2010.02.04 Dan
;;;             : * Added the fortress, explosion, bonus, and vlner objects to
;;;             :   be seen by the model and changed it so that some items are
;;;             :   now "dynamic" meaning they may or may not be visible in 
;;;             :   every update.  The assumption is that an item is not visible
;;;             :   if it is dynamic unless the update has an instance for it.
;;; 2010.02.11 Dan
;;;             : * When the fortress explodes make that visible to the model --
;;;             :   assume if there was a fortress but isn't now then a 
;;;             :   fortress-explosion is to be shown.
;;;             :   
;;; 2010.02.15 Dan
;;;             : * Tweak the kind of the bonus item so that the visual-search
;;;             :   buffer can detect when it's new since the finsts (attended)
;;;             :   info isn't available in the chunks to test.
;;; 2010.02.22 Dan 
;;;             : * Make the update-world event non-maintenance because if the
;;;             :   model isn't actively tracking (moving attention to the bonus
;;;             :   item for example) then there are no model events to bring
;;;             :   conflict-resolution "back" to and it can still get stuck
;;;             :   with a long delay from temporal or declarative.
;;; 2010.02.23 Dan
;;;             : * Added the 2-handed and attend-and-track loaders to the end.
;;; 2010.03.01 Dan
;;;             : * Changed get-internal-real-time to (win::gettickcount) in
;;;             :   the logging of the socket data for the higher resolution
;;;             :   that it provides.  This does make it ACL specific for now.
;;; 2010.03.04 Dan
;;;             : * Modified show-ship-trial to display the mine as well when
;;;             :   it exists.
;;; 2010.03.11 Dan
;;;             : * Fixed some issues with a fortress explosion being "visible"
;;;             :   at the start of a trial if it was available on the previous
;;;             :   one and the ship's explosion continuing to be seen during
;;;             :   the 1 second of down-time.
;;; 2010.03.15 Dan
;;;             : * Adding a slot to the ship to indicate when it hits the 
;;;             :   fortress hex.  That will stay set until there is a change
;;;             :   to the ships velocity vector.
;;; 2010.03.19 Dan
;;;             : * Fixed the fortress location info -- it's at 355,315.
;;;             : * Added code to check whether or not a ship's shot will hit
;;;             :   the mine and to do the prediction to see if it will ever
;;;             :   be in a position to shoot it given it's current drift and
;;;             :   orientation or if it will need to turn.
;;; 2010.03.26 Dan
;;;             : * Scheduling the proc-display calls in the vis-loc-to-obj 
;;;             :   method because otherwise bad things occur with respect to
;;;             :   chunk deletion...
;;; 2010.03.31 Dan
;;;             : * Modifying the mine side to be ahead/behind instead of
;;;             :   left/right/ahead so the model can be more sensitive
;;;             :   to ones coming from behind.  The prediction code now also
;;;             :   computes a turn to both sides and stops at the closer one.
;;;             : * Now also determines when it needs to start dealing with
;;;             :   the mine based using the distance as the key since the
;;;             :   temporal is already in use.  The prediction has a slot for
;;;             :   the mine distance at which the model needs to switch focus
;;;             :   to the mine based on the "soonest" it can shoot it if the
;;;             :   ship doesn't change velocity.
;;; 2010.04.01 Dan
;;;             : * Decreasing the preference to drift from the mine prediction
;;;             :   code.  Now the drift only is if it's 9 or fewer ticks and
;;;             :   the drift instead is if it's a dif of 5.
;;; 2010.04.27 Dan
;;;             : * Fixed an issue with shooting at the mine where the prediction
;;;             :   could have it turn the wrong way when it comes in at about 90
;;;             :   degrees to the ship orientation.
;;; 2010.05.05 Dan
;;;             : * Modified update-world so that the fortress-hit is only
;;;             :   visible when there is still a fortress around.
;;; 2010.05.13 Dan
;;;             : * To improve running performance added the redefinition of
;;;             :   the ACT-R model structure to tweak the hash-table settings.
;;; 2010.06.15 Dan
;;;             : * Adjusted the mine prediction code to increase the action
;;;             :   time for imaginal to be 300ms so, now assume 12 ticks before
;;;             :   any turning.
;;; 2010.06.17 Dan
;;;             : * Adjusted the mine prediction code to increase the action
;;;             :   time for imaginal to be 250ms so, now assume 10 ticks before
;;;             :   any turning.
;;; 2010.06.23 Dan
;;;             : * Reverted to a 200ms action for the prediction and added 
;;;             :   a new imaginal action for returning to orbit after shooting
;;;             :   a mine.
;;; 2010.09.10 Dan
;;;             : * Added a speed-change slot to the ship and changed the 
;;;             :   "reassess" distance (the hit-dist) from hit -25 to hit - 50
;;;             :   so the model waits a little longer before trying to fix
;;;             :   things.
;;; 2010.09.17 Dan
;;;             : * Changed the reassess from -50 to -90 to try waiting even
;;;             :   a little more...
;;; 2010.11.24 Dan
;;;             : * Cleaned up a few things for distribution.  Basically, removed
;;;             :   the ACL specific code for recording events and drawing debuging
;;;             :   info eventhough nobody would have been using that anyway.
;;; 2011.08.17 Dan 
;;;             : * Updated to work with ACT-R r1113.  Required changing the
;;;             :   slack function to accept two parameters.
;;; 2011.09.15 Dan [2.0]
;;;             : * Start of the rewrite to work with the new interface Shawn
;;;             :   created instead of the old one Ben and I put together.
;;; 2011.09.20 Dan
;;;             : * Only pushing data onto the incoming record from both
;;;             :   places for now i.e. both incoming and outgoing are on the
;;;             :   incoming list.  It also has the model time as the first
;;;             :   element.
;;; 2011.09.26 Dan
;;;             : * Adding some hook functions for data recording purposes.
;;;             :   :mid-fixation gets called at the middle time of the
;;;             :    fixation screens with the last mid-fixation time and the 
;;;             :    current mid-fixation time as the parameters.
;;;             :   :game-start gets called with the current time when the
;;;             :    first game event occurs.
;;;             :   :game-end gets called with the last game start time and the
;;;             :    current time when the score display occurs.
;;; 2011.09.28 Dan
;;;             : * Updated to use the additions Shawn has added to fill in the
;;;             :   ship details and add the collisions info.
;;; 2015.08.13 Dan [3.0]
;;;             : * Updating to work with ACT-R 7.
;;;             : * Eliminated the sf-object chunk-type since it wasn't really
;;;             :   used for anything and make bonus and trial-end chunk-types
;;;             :   unique by adding a default slot matching the type name with
;;;             :   a value of t. 
;;; 2015.08.20 Dan
;;;             : * Removed a call to update-world in collect-sf-incoming-data
;;;             :   because it seemed unnecessary and could cause problems 
;;;             :   since it would be called from the "other" thread.
;;;             : * Removed an unnecessary test from the :quit situation so that
;;;             :   it always stops the model when the task is over.
;;; 2015.08.24 Dan
;;;             : * Start of reworking the connection logic to make sure things
;;;             :   are repeatable because there appears to be some discrepancies
;;;             :   with when an event occurs in the game even with a deterministic
;;;             :   model and game.  I think it's because of how the slack hook,
;;;             :   background data collection process, and key handler methods
;;;             :   interact, but can't really pin it down.  Here's an example
;;;             :   from two traces of the same game and model that shows the
;;;             :   issue (the model time is 20s ahead of the game time because
;;;             :   of the info screens).  The model actions should be associated
;;;             :   with the next game update time (the update-world calls).
;;;             :   The keypress at time 35.918 should happen at 35.939 since
;;;             :   that is the next game update time, but in the second log it
;;;             :   isn't registered until two updates later at 36.005.
#|

Model trace (same for both games):

    35.741   SF                     UPDATE-WORLD
    35.741   VISION                 MOD-BUFFER-CHUNK VISUAL
    35.741   PROCEDURAL             CONFLICT-RESOLUTION
    35.743   MOTOR                  PREPARATION-COMPLETE style DELAYED-PUNCH hand RIGHT
    35.743   PROCEDURAL             CONFLICT-RESOLUTION
    35.763   MOTOR                  INITIATION-COMPLETE style DELAYED-PUNCH hand RIGHT
    35.763   PROCEDURAL             CONFLICT-RESOLUTION
    35.767   TEMPORAL               Incrementing time ticks to 21
    35.767   TEMPORAL               MOD-BUFFER-CHUNK TEMPORAL
    35.767   PROCEDURAL             CONFLICT-RESOLUTION
    35.773   MOTOR                  OUTPUT-KEY #(7 4)
    35.773   PROCEDURAL             CONFLICT-RESOLUTION
    35.774   SF                     UPDATE-WORLD
    35.774   VISION                 MOD-BUFFER-CHUNK VISUAL
    35.774   PROCEDURAL             CONFLICT-RESOLUTION
    35.807   SF                     UPDATE-WORLD
    35.807   VISION                 MOD-BUFFER-CHUNK VISUAL
    35.807   PROCEDURAL             CONFLICT-RESOLUTION
    35.807   PROCEDURAL             PRODUCTION-SELECTED THRUST-FAST-LONG
    35.807   PROCEDURAL             BUFFER-READ-ACTION GOAL
    35.807   PROCEDURAL             BUFFER-READ-ACTION VISUAL
    35.807   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    35.807   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL-LEFT
    35.837   MOTOR                  RELEASE-KEY #(7 4)
    35.840   SF                     UPDATE-WORLD
    35.840   VISION                 MOD-BUFFER-CHUNK VISUAL
    35.846   TEMPORAL               Incrementing time ticks to 22
    35.846   TEMPORAL               MOD-BUFFER-CHUNK TEMPORAL
    35.857   PROCEDURAL             PRODUCTION-FIRED THRUST-FAST-LONG
    35.857   PROCEDURAL             MODULE-MOD-REQUEST GOAL
    35.857   PROCEDURAL             MODULE-REQUEST MANUAL
    35.857   GOAL                   MOD-BUFFER-CHUNK GOAL
    35.857   PROCEDURAL             CLEAR-BUFFER MANUAL
    35.857   MOTOR                  DELAYED-PUNCH HAND LEFT FINGER MIDDLE DELAY 0.16
    35.857   PROCEDURAL             CONFLICT-RESOLUTION
    35.873   SF                     UPDATE-WORLD
    35.873   VISION                 MOD-BUFFER-CHUNK VISUAL
    35.873   PROCEDURAL             CONFLICT-RESOLUTION
    35.888   MOTOR                  PREPARATION-COMPLETE style DELAYED-PUNCH hand LEFT
    35.888   PROCEDURAL             CONFLICT-RESOLUTION
    35.906   SF                     UPDATE-WORLD
    35.906   VISION                 MOD-BUFFER-CHUNK VISUAL
    35.906   PROCEDURAL             CONFLICT-RESOLUTION
    35.908   MOTOR                  INITIATION-COMPLETE style DELAYED-PUNCH hand LEFT
    35.908   PROCEDURAL             CONFLICT-RESOLUTION
    35.914   MOTOR                  FINISH-MOVEMENT style DELAYED-PUNCH hand RIGHT
    35.914   PROCEDURAL             CONFLICT-RESOLUTION
    35.914   PROCEDURAL             PRODUCTION-SELECTED SHOOT-FAST-SECOND
    35.914   PROCEDURAL             BUFFER-READ-ACTION GOAL
    35.914   PROCEDURAL             BUFFER-READ-ACTION VISUAL
    35.914   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    35.914   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL-LEFT
    35.914   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL-RIGHT
    35.914   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    35.914   PROCEDURAL             BUFFER-SEARCH VISUAL-SEARCH
    35.918   MOTOR                  OUTPUT-KEY #(3 4)
    35.932   TEMPORAL               Incrementing time ticks to 23
    35.932   TEMPORAL               MOD-BUFFER-CHUNK TEMPORAL
    35.939   SF                     UPDATE-WORLD
    35.939   VISION                 MOD-BUFFER-CHUNK VISUAL
    35.964   PROCEDURAL             PRODUCTION-FIRED SHOOT-FAST-SECOND
    35.964   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
    35.964   PROCEDURAL             MODULE-MOD-REQUEST GOAL
    35.964   PROCEDURAL             MODULE-REQUEST MANUAL
    35.964   GOAL                   MOD-BUFFER-CHUNK GOAL
    35.964   PROCEDURAL             CLEAR-BUFFER VISUAL-SEARCH
    35.964   PROCEDURAL             CLEAR-BUFFER MANUAL
    35.964   MOTOR                  DELAYED-PUNCH HAND RIGHT FINGER INDEX DELAY 0.09
    35.964   PROCEDURAL             CONFLICT-RESOLUTION
    35.972   SF                     UPDATE-WORLD
    35.972   VISION                 MOD-BUFFER-CHUNK VISUAL
    35.972   PROCEDURAL             CONFLICT-RESOLUTION
    35.999   MOTOR                  PREPARATION-COMPLETE style DELAYED-PUNCH hand RIGHT
    35.999   PROCEDURAL             CONFLICT-RESOLUTION
    36.005   SF                     UPDATE-WORLD


Game 1 log:

15774 1440184862.784000 hold-fire,missile-fired
15807 1440184862.789000 
15840 1440184862.792000 release-fire
15873 1440184862.796000 
15906 1440184862.800000 hit-fortress,vlner-increased
15939 1440184862.804000 hold-thrust
15972 1440184862.809000 
16005 1440184862.813000 

game 2 log:

15774 1440187421.873000 hold-fire,missile-fired
15807 1440187421.878000 
15840 1440187421.881000 release-fire
15873 1440187421.885000 
15906 1440187421.889000 hit-fortress,vlner-increased
15939 1440187421.892000 
15972 1440187421.896000 
16005 1440187421.901000 hold-thrust
|#
;;; 2015.08.27 Dan
;;;             : * Changed the key handlers to use push-last because they need
;;;             :   to be sent in the order they're generated in the event that
;;;             :   a down and up happen in the same time tick.
;;; 2015.09.01 Dan
;;;             : * Adding :sf-key-down-hook and :sf-key-up-hook which will be 
;;;             :   passed the world and the key at the "next" update i.e. after
;;;             :   the proc-display has actually happened so that the effects
;;;             :   will have been registered in the percepts and game info.
;;; 2015.09.25 Dan
;;;             : * New screen-types: instructions - requires keypress
;;;             :                     basic-task - doesn't require keypress?
;;; 2015.09.28 Dan
;;;             : * Fix some of the end game transition code for the new screens
;;;             :   so the model starts new games correctly.
;;; 2015.09.30 Dan
;;;             : * The key hook function(s) for a key will continue to be called
;;;             :   until a true value is returned from the hook call for that key.
;;; 2015.10.01 Dan
;;;             : * Adjust for most recent game version which now has the end
;;;             :   score screen as "score" instead of "total_score".
;;; 2015.10.06 Dan
;;;             : * Load the analyze-position file as well from the space-fortress-modules
;;;             :   directory.
;;; 2015.10.07 Dan
;;;             : * Add a few things to the reset function that weren't there
;;;             :   previously and affected repeatability...
;;; 2015.10.19 Dan
;;;             : * Track ship deaths and fortress kills per game.
;;; 2016.07.07 Dan [3.1]
;;;             : * Added a hook function to the module :sf-data-hook which
;;;             :   if set to a function will pass the data line read in to that
;;;             :   function.
;;;             : * Set the value slot of the explosion feature to indicate why
;;;             :   it occurred: inner (hit inner hex), outer (hit outer hex),
;;;             :   or shell (shell hit ship) and set the explosion object
;;;             :   chunk's status slot accordingly (leaving the value as explosion
;;;             :   so there's a general test and to be compatible with older models).
;;;             :   If none of those are true then the value/status will just be
;;;             :   explosion. Still changing the explosion-loc to kind debris 
;;;             :   when it's attended.
;;; 2016.07.08 Dan [3.2]
;;;             : * Cheat on the location of an explosion so that the vision 
;;;             :   module should always re-encode that as the feature when 
;;;             :   tracking the ship.
;;;             : * Also override the default value of :tracking-clear so that
;;;             :   the model should always see the explosions when tracking the
;;;             :   ship.
;;; 2016.07.11 Dan [3.3]
;;;             : * Add features for a single shell and single missile. 
;;;             :   If there are any shells then the nearest one to the ship 
;;;             :   can be found with a kind shell and if there are any missiles
;;;             :   the nearest one to the fortress is found with kind missile.
;;;             :   Only representing a single at this point to avoid issues
;;;             :   with excessive chunk creation and needing to map multiple
;;;             :   items to the "same" feature to allow tracking -- thus trying
;;;             :   to track either of these only really works as long as there's
;;;             :   only one available.
;;;             : * Add a case for the config screen type which just does nothing
;;;             :   to avoid a warning for the unexpected type.
;;;             : * Fixed a bug with the explosion-loc going from explosion 
;;;             :   to debris and then back to explosion for a single ship death.
;;;             : * Added the version check.
;;;             : * Added the values to the shell and missile chunks at the start.
;;; 2016.07.13 Dan [3.4]
;;;             : * Use the total-score notice with a delay of 0 to reset the
;;;             :   the last-time value to 0 since there isn't a basic-game or
;;;             :   fmri-game notice now.
;;;             : * The score screen also needs to reset the timer and add a 
;;;             :   dummy delay of 1 second for "mode 2" servers.
;;;             : * But only if the screen type is delay...
;;; 2016.07.14 Dan [3.5]
;;;             : * Set the value of the missile and shell locations to the count
;;;             :   of the corresponding item visible, and the count slot of the
;;;             :   objects.
;;;             :  [4.0]
;;;             : * Turns out for the newest "mode 2" servers there is no type
;;;             :   delay on the score updates.  So, need to add the 1 second
;;;             :   delay to only the events type.  Since this only works with
;;;             :   the newer servers updating the version significantly.
;;;             : * Remove an unnecessary proc-display in mode 1 servers to
;;;             :   avoid a warning about multiple proc-display messages.
;;; 2016.07.22 Dan
;;;             : * Have vis-loc-to-obj only schedule one proc-display to avoid
;;;             :   some warnings.
;;;             : * Only schedule a proc-display if there isn't already one
;;;             :   either waiting because of a device lock or already scheduled
;;;             :   at the same time by the module (replacing last-proc-handled
;;;             :   with last-proc-scheduled to deal with that since the handled
;;;             :   isn't being used for the current synchronous game interface).
;;;             : * Updated the chunk and chunk-type definitions to provide a
;;;             :   label slot in all of the objects to indicate their "type".
;;; 2016.07.25 Dan
;;;             : * Added a pnts-loc and pnts chunk with label points for the
;;;             :   score.  The x,y are 245,700 which probably isn't right, but
;;;             :   puts it on the other side of screen center opposite vlner text.
;;; 2016.09.06 Dan
;;;             : * Updated the written-for-act-r-version to 7.1 since that's the
;;;             :   current one in the repository and avoids a warning since the
;;;             :   major version has changed.
;;;             : * Added support for the staircase game by pulling the bighex
;;;             :   and smallhex values from the input and using those to create
;;;             :   features for the visual chunk.  There are 6 new slots:
;;;             : 
;;;             :   - dist-to-bighex & dist-to-smallhex:
;;;             :      distance from current ship point to nearest segment of
;;;             :      the indicated hex measured in pixels
;;;             :   
;;;             :   - travel-dist-to-bighex & travel-dist-to-smallhex: 
;;;             :      distance from current ship position to the indicated
;;;             :      hex along its current trajectory measured in pixels
;;;             :      or 9999 if it won't intersect the hex
;;;             :   
;;;             :   - travel-time-to-bighex & travel-time-to-smallhex: 
;;;             :      time in seconds until the ship hits the indicated
;;;             :      hex along its current trajectory measured in seconds
;;;             :      or 9999 if it won't hit
;;; 2016.09.08 Dan [5.0]
;;;             : * Added a play-sf-games which stops things at the score screen
;;;             :   after the indicated number of games have occurred.
;;;             : * Added the ability to set a specific name for the model's data
;;;             :   directory.  If not provided it uses a number written into a
;;;             :   file to name it model_#.  The file is called sf-model-number.txt
;;;             :   and exists in the same directory this file was loaded from.
;;;             : * Cleaned up the communication because there was some skewing
;;;             :   of the updates since the model sent a drawing command before
;;;             :   it read the first config line, and then sent an extra continue
;;;             :   as a result.
;;;             :
;;;             :   == That means the data files generated by a deterministic ==
;;;             :   == model using this update will not be identical to those ==
;;;             :   == using the older version!                               ==
;;;             :
;;; 2016.09.09 Dan
;;;             : * Removed a bunch of old code and object slots that aren't
;;;             :   being used anymore to clean things up.
;;; 2016.09.13 Dan
;;;             : * Added code to work with older version of the game server too
;;;             :   (the refactored one that I have) just to be safe.  The changes
;;;             :   to do that require accepting a nil result for changing the id
;;;             :   as long as the :id value was updated and assuming that the
;;;             :   hex sizes are 200 and 40 if they aren't provided.
;;; 2017.03.24 Shawn Betts
;;;             : * Added code to record end of game stats in the game-stats
;;;             :   slot of the module: (game-stats (get-module :sf)).
;;;             :   The stats are formatted as a plist but are also in the same 
;;;             :   order as the game-events spreadsheets.
;;; 2017.05.04 Dan
;;;             : * When it's running on the cluster via cluster-run use the
;;;             :   *cluster-run-start* and *cluster-run-id* to create the
;;;             :   model's id.
;;; 2017.12.01 Dan [5.1]
;;;             : * Provide a parameter for setting a custom "ship object" chunk
;;;             :   creation function.  The :ship-feature parameter can be set to
;;;             :   a function which will be called with the current data object
;;;             :   from the game.  It should return a list of slot-value pairs
;;;             :   that will be used to create the ship feature e.g.
;;;             :   (x 100 y 100 speed 1.44 angle .43).
;;; 2018.01.19 Dan
;;;             : * Added a new parameter to go along with the previous one 
;;;             :   called :clear-ship-feat which defaults to t, but if set to
;;;             :   nil does not clear the previous ship chunk slots before
;;;             :   applying the modifications returnd from the ship-feature hook.
;;; 2018.02.04 Shawn
;;;             :   * Add support for an external-process server.
;;; 2018.06.21 Dan [6.0]
;;;             : * Adjust play-sf-games to reconnect and send a new condition
;;;             :   if continuing but with a different condition which is now
;;;             :   stored in the module.
;;; 2018.06.22 Dan
;;;             : * Removing the old update-world when changing conditions.  It's
;;;             :   probably safe to assume it'll be in the right place since 
;;;             :   that's how a normal continue works, but this is safer incase
;;;             :   something changes with the game init code.
;;; 2018.06.27 Dan
;;;             : * Added the previous two updates into the version that works
;;;             :   with either the tcp or cluster game server.
;;;             : * ACL doesn't like condition as an accessor so change that
;;;             :   to game-condition.
;;; 2019.04.26 Dan [7.0]
;;;             : * Start the transition to ACT-R 7.x.
;;;             :   - don't load unneeded extensions and use require-extra for
;;;             :     the ones that are already included.
;;;             :   - get rid of device methods and just add some monitoring
;;;             :     commands globally (assume it's not going to need to run
;;;             :     other tasks after loading this)
;;;             :   - assume that it's running single threaded and don't bother
;;;             :     with locks.
;;;             : * Cut all the mine aiming and reorbiting calculations.
;;;             : * Remove the key-up and key-down hook functions (why were they
;;;             :   added?).
;;;             : * Removed the bonus feature timer since the :attended new test
;;;             :   will work with the search buffer and it'll be a new feature
;;;             :   each time.
;;;             : * Track and install the key monitors in the code instead of at
;;;             :   the top level since release-key isn't available at that point
;;;             :   and should release them to be clean.
;;; 2020.01.22 Dan [7.1]
;;;             : * Run-until-condition and scheduleing require names instead of
;;;             :   lambdas now.
;;; 2020.01.23 Dan
;;;             : * Updated the written-for to indicate 7.15 now.
;;;             : * Fixed a bug in the updated play-sf-games.
;;; 2020.02.05 Dan
;;;             : * Changed config-set-settings to send the correct command for
;;;             :   toggling the display on or off.
;;; 2020.04.13 Dan
;;;             : * Fixed the issue of the ship's chunk-type setting the hit-hex
;;;             :   instead of hex-hit (there's a discrepancy between the chunk
;;;             :   and object, but for backward compatibility continue to use
;;;             :   the same chunk slot name).
;;; 2020.07.01 Dan [8.0]
;;;             : * Moved default chunk creation from reset to create (can't 
;;;             :   move chunk-types since they depend on vision which may not
;;;             :    have created it's own yet).
;;;             : * Added a parameter :use-vision that toggles whether the
;;;             :   visual features are created and used (defaults to nil).
;;;             : * Moved the post-reset function to tertiary position so that
;;;             :   user setting of :use-vision has been processed.
;;;             : * If vision isn't being used, need to make update-world a
;;;             :   model event for ACT-R to respond since there's no vision
;;;             :   module actions to respond to.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Module and device for connecting ACT-R models to the special python version
;;; of the space fortress task which we have.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; Do a version check since it should be run with the updated vision module.

(written-for-act-r-version "7.15" (format nil "This Space Fortress module~%           Requires ACT-R 7.15 or newer!"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Redefine the ACT-R model structure so that the hash-table is more appropriate
;;; for this task -- reduces the run time for a day from ~6 minutes to just over
;;; 2 with (play-sf (* 210.01 16) :speed 150).
;;;
;;; [3.0] Just set the system parameters now instead of redefining the structure
;;; since that's why they were added.

(ssp :mcts 45000 :mctrt .75)

;;; Variables to hold the connection details.

;; (defparameter *host-address* "127.0.0.1" "Address/name of the machine running the server")
;; (defparameter *port-for-server* 3000 "default port for the server")
;; (defparameter *server-type* 'sfapi-tcp-client "default server type to use. Valid values are SFAPI-TCP-CLIENT and SFAPI-EXTERNAL-PROCESS-CLIENT.")

;;;;;;;;;;;;;;;;;;;;;
;;; Tools 

;;; Communication to the game is via text based commands on one line.  The sending functions are 
;;; responsible for proper formatting.

(defconstant *cr/lf* (format nil "~C~C" #\cr #\lf))

(defmacro server-write-line (world string &rest args)
  "Write a CRLF-terminated line"
  `(if (or (null (server ,world))
           (not (sfapi-connected-p (server ,world))))
       (print-warning "Can't send command {~S} because connection closed" 
                   (format nil ,string ,@args))
     (let ((string-to-write (format nil ,string ,@args)))
       (when (record-outgoing ,world)
         (push (list :out (mp-time) (get-internal-real-time) string-to-write)
               (data-record ,world)))

       (sfapi-write (server ,world) 
                    (concatenate 'string string-to-write *cr/lf*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The UT device code

;;; Define a class for the device which will also be the instance for the module.

(defclass sf-world () 
  (;; connection info
   (host :initform nil :accessor sf-host)
   (port :initform nil :accessor sf-port)
   (server-type :initform nil :accessor sf-server-type)
   (server :initform nil :accessor server)
   (server-current-screen :initform nil :accessor server-current-screen)
   (game-state :initform nil :accessor game-state)
   
   ;; communication logging
   (collect-incoming :initform nil :accessor collect-incoming)
   (record-outgoing :initform nil :accessor record-outgoing)
   (data-record :initform nil :accessor data-record)

   ;; visual feature processing
   (all-locs :accessor all-locs :initform (make-hash-table :test 'eq) )
      
   ;; flag for whether visual info needed
   (use-vision :accessor use-vision :initform nil)
   
   ;; hacks for visual info missing or otherwise in need of special care
   (fortress-there :initform nil :accessor fortress-there)
   (f-hit-counter :initform nil :accessor f-hit-counter)
   (hit-hex :initform nil :accessor hit-hex)
   (last-v :initform nil :accessor last-v)
   (last-dist :initform nil :accessor last-dist)
   (ship-pos :initform nil :accessor ship-pos)
   (mine-visible :initform nil :accessor mine-visible)
   (fort-exists :initform nil :accessor fort-exists)
   (fort-pos :initform (vector 355 315) :accessor fort-pos)
   
   ;; actions to send to the game
   (responses :initform nil :accessor responses)
   (auto-response :initform nil :accessor auto-response)
   
   ;; user hook parameters
   (fix-hook :initform nil :accessor fix-hook)
   (start-hook :initform nil :accessor start-hook)
   (end-hook :initform nil :accessor end-hook)
   (data-hook :initform nil :accessor data-hook)
   
   ;; track some game stats as it plays
   (ship-alive :initform nil :accessor ship-alive)
   (game-stats :initform (list (list 0 0)) :accessor game-stats)
   (fortress-killed :initform nil :accessor fortress-killed)
   
   ;; connection and running control
   (last-time :initform 0 :accessor last-time)
   (name :initform nil :accessor name)
   (game-count :initform 0 :accessor game-count)
   (game-limit :initform -1 :accessor game-limit)
   (change-drawing-mode :initform nil :accessor change-drawing-mode)
   
   (key-handlers :initform nil :accessor key-handlers)
   
   ;; user config
   (ship-feat :initform nil :accessor ship-feat)
   
   (game-condition :initform nil :accessor game-condition)
   (clear-ship-feat :initform nil :accessor clear-ship-feat)))
  


;;; The keyboard signals output-key and release-key.

(defun sf-handle-keypress (model key-string)
  (declare (ignore model))
  (let ((world (get-module :sf))
        (key (char key-string 0)))
    (push-last (cons 'keydown (model-key-to-sf-key key)) (responses world))))

(defun sf-handle-keyrelease (model key-string)
  (declare (ignore model))
  (let ((world (get-module :sf))
        (key (char key-string 0)))
    (push-last (cons 'keyup (model-key-to-sf-key key)) (responses world))))


(add-act-r-command "sf-key-down" 'sf-handle-keypress "SF key down monitor")
(add-act-r-command "sf-key-up" 'sf-handle-keyrelease "SF key up monitor")


;;; To keep things simple it's just assuming the hand positions are over
;;; the correct keys.  The model may set them that way if it wants, but
;;; it will not affect how it plays.
;;;
;;; [2.0] These are now mapped to the keycodes needed instead of the key names.
;;; with the current actions in the config being:  a,w,d for movement (left ring, middle, index respectively)
;;; space for fire (right index) and j for IFF (right middle)
;;; all others are mapped to z as a default.
;;;
;;; [3.0] The model presses are now reported as keys so need to change the
;;; mapping appropriately with the assumption that the models hands will
;;; just be on the home row.  Thus s,d,f are left, thrust, right and
;;; j,k,l are fire, iff, and bonus.
  
  
(defun model-key-to-sf-key (key)
  (case key
    (#\d 119)
    (#\s  97)
    (#\f 100)
    (#\j  32)
    (#\k 106)
    (#\l 107)
    (t  122)))



;;; Send the model's actions and any automatic responses that
;;; are generated for some of the between game screens used in
;;; the fMRI task.

(defun send-all-pending-model-actions (world)
  
  (dolist (x (responses world))
    (server-write-line world "~a ~d 0" (car x) (cdr x)))
  
  (setf (responses world) nil)
  
  (let (new)
    
    (dolist (x (auto-response world))
      (server-write-line world "~a ~d 0" (car x) (cdr x))
      (when (eq (car x) 'keydown)
        (push-last (cons 'keyup (cdr x)) new)))
    
    (setf (auto-response world) new)))
  
;;; Read a line of data from the server skipping blank lines
;;; and terminating the run if there's an error encountered.

(defun get-line-of-data-from-game (world)
  (loop
    (multiple-value-bind (line err)
        (ignore-errors (sfapi-read (server world)))

      (when (subtypep (type-of err) 'condition)
        (setf (game-state world) 'disconnected)
        (print-warning "Error reading from game server.  Cannot continue.")
        (schedule-break-relative 0 :priority :max :details "Game connection error" :time-in-ms t)
        (return nil))
      (unless (string= line "")
        (return line)))))


(defun default-ship-features (ship data)
  (let ((x (getf ship :x))
        (y (getf ship :y))
        (angle (getf ship :angle))
        (vel (getf ship :speed))
        (dist (getf ship :distance-from-fortress))
        (vdir (getf ship :vdir))
        (orient (getf ship :orientation))
        (vx (getf ship :vx))
        (vy (getf ship :vy))
        (bighex (aif (getf data :bighex) it 200))
        (smallhex (aif (getf data :smallhex) it 40))
        )
    
    (list 'isa 'ship
          'screen-x x
          'screen-y y
          'x x
          'y y
          'vx vx
          'vy vy
          'angle angle
          'vel vel
          'dist dist
          'vdir (if (= vel 0.0) angle vdir)
          'orientation orient
          'hex-dist nil 
          'travel-time-to-bighex (travel-time-to-hex vel x y vx vy :radius bighex)
          'travel-time-to-smallhex (travel-time-to-hex vel x y vx vy :radius smallhex)
          'dist-to-bighex (dist-to-hex x y :radius bighex)
          'dist-to-smallhex (dist-to-hex x y :radius smallhex)
          'travel-dist-to-bighex (travel-dist-to-hex vel x y vx vy :radius bighex)
          'travel-dist-to-smallhex (travel-dist-to-hex vel x y vx vy :radius smallhex))))




(defstruct feature previous now id)

(defun incf-game-count (world) (incf (game-count world)))

;;; Handle all the game updating in an event that gets scheduled to occur based
;;; on the times provided.  This keeps things synchronous and avoids issues with
;;; a separate thread collecting data asynchronously.

(defun update-world (world)
  
  ;; send everything that's happened since the last update
  (if (eq (server-current-screen world) 'score)
      ;; The score screen does not accept key events
      (setf (responses world) nil
            (auto-response world) nil)
      (send-all-pending-model-actions world))
  
  ;; Always send a continue.
  ;; The next update will be scheduled based on the
  ;; time/delay from the game data that's read here.
  
  (server-write-line world "continue")
  
  (let ((state-line (get-line-of-data-from-game world)))
    
    (unless state-line (return-from update-world))
    
    (let* ((data (read-from-string state-line))
           (delay (if (string-equal (getf data :mode) "delay")
                      (getf data :delay_duration)
                    0)))
      
      ;; record the data and send it to the hook if needed
      
      (when (collect-incoming world)
        (push (list :in (mp-time) (get-internal-real-time) data) (data-record world)))
      
      (awhen (data-hook world) (ignore-errors (funcall it data)))
      
      
      ;; mark everything as not visible now and record if it was visible last update
      
      (when (use-vision world)
        (maphash (lambda (key feat)
                   (declare (ignore key))
                   (setf (feature-previous feat) (feature-now feat))
                   (setf (feature-now feat) nil))
                 (all-locs world)))
     
      
      ;; Update the features and time info based on the type of info this is
      
      (setf (server-current-screen world) (read-from-string (getf data :screen-type)))
      
      (case (server-current-screen world)
        
        (progress ;; just ignore it
         )
        
        (basic-task ;; press z to skip it and reset time offset
         (push-last (cons 'keydown 122) (auto-response world))
         (setf (last-time world) 0)
         )
        
        (instructions ;; press z to skip it
         (push-last (cons 'keydown 122) (auto-response world))
         )
        
        (wait-caret ;; press the ^ key to skip it
         (push-last (cons 'keydown 94) (auto-response world))
         )
        
        (fixation ;; assume a 10 second delay if none specified
         (when (zerop delay) 
           (setf delay 10000))
         )
        
        (fmri-task ;; press z to advance and record the visual
                   ;; features for the game type and reset time offset
         
         (push-last (cons 'keydown 122) (auto-response world))
         
         (when (zerop delay) ;; assume 10 second delay if none specified
           (setf delay 10000))
         
         
         (when (use-vision world)
           (let ((cond1 (gethash 'cond1 (all-locs world)))
                 (cond2 (gethash 'cond2 (all-locs world))))
             
             (setf (feature-now cond1) t)
             (setf (feature-now cond2) t)
             (setf (feature-id cond1) (car (add-visicon-features `(isa labeled-text label condition screen-x 200 screen-y 500 kind text value (text ,(if (getf data :mines) "+mine" "-mine"))))))
             (setf (feature-id cond2) (car (add-visicon-features `(isa labeled-text label condition screen-x 400 screen-y 500 kind text value (text ,(if (getf data :fortress) "+fort" "-fort"))))))
             ))
         
         (unless (getf data :mines)
           (setf (mine-visible world) nil))
           
         (if (getf data :fortress)
             (setf (fort-exists world) t)
           (setf (fort-exists world) nil))
         
         (setf (last-time world) 0))
        
        
        (foe-mines ;; press z to advance and record the visual
                   ;; features for the mine letters 
           
         (push-last (cons 'keydown 122) (auto-response world))
         
         (when (zerop delay) ;; assume 10 second delay if none specified
           (setf delay 10000))
         
         ;; hack to make sure fortress isn't considered blown up
         (setf (fortress-there world) nil)
         
         (when (use-vision world)
           (let ((letters (getf data :letters))
                 (foe1 (gethash 'foe1 (all-locs world)))
                 (foe2 (gethash 'foe2 (all-locs world)))
                 (foe3 (gethash 'foe3 (all-locs world))))
             
             (setf (feature-now foe3) t)
             (setf (feature-now foe2) t)
             (setf (feature-now foe1) t)
             (setf (feature-id foe1) (car (add-visicon-features `(isa labeled-text label foe screen-x 200 screen-y 500 kind text value (text ,(string (aref letters 0)))))))
             (setf (feature-id foe2) (car (add-visicon-features `(isa labeled-text label foe screen-x 300 screen-y 500 kind text value (text ,(string (aref letters 1)))))))
             (setf (feature-id foe3) (car (add-visicon-features `(isa labeled-text label foe screen-x 400 screen-y 500 kind text value (text ,(string (aref letters 2)))))))
             )))
        
        (total-score ;; gets sent twice one with a delay and one without
                     ;; in the delay version provide the visual feature
                     ;; so the model sees the end of game and clear some 
                     ;; of the hack flags
                     ;; In the no delay version hit z to skip it, record results,
                     ;; and schedule an event for 1 second from not to note the
                     ;; game ended.
           
         ;; old hack left in
         (setf (mine-visible world) nil) 
         (setf (fortress-there world) nil)
         
         (cond ((zerop delay)
                (push-last (cons 'keydown 122) (auto-response world))
                (push-last (getf data :raw-pnts) (first (game-stats world)))
                (push-last (getf data :bonus) (first (game-stats world)))
                (push-last (getf data :total-bonus) (first (game-stats world)))
                (push-last (append (getf data :base-stats) (getf data :event-stats) (getf data :input-stats)) (first (game-stats world)))
                
                (push (list 0 0) (game-stats world))
                (schedule-event-relative 1000 'incf-game-count :params (list world) :priority :max :time-in-ms t :maintenance t :output nil)
                (setf (last-time world) 0))
               
               (t
                (when (use-vision world)
                  (let ((f  (gethash 'trial-end (all-locs world))))
                    
                    (setf (feature-now f) t)
                    
                    (setf (feature-id f) (car (add-visicon-features `(isa trial-end screen-x 355 screen-y 315)))))
                  
                  (setf (hit-hex world) nil)
                  (setf (last-v world) nil)))))
        
        
        (score ;; there's only one score message for the current game servers
               ;; when using model_interface 2 which is all this will handle now
               ;; this might cause problems if running with an older space fortress
               ;; game server.
               ;; 
               ;; Show the trial-end feature to the model, record the
               ;; game stats, and schedule an event to mark the game
               ;; as complete after 1 second.
           
         (when (use-vision world)
           (let ((f (gethash 'trial-end (all-locs world))))
             (setf (feature-now f) t)
             (setf (feature-id f) (car (add-visicon-features `(isa trial-end screen-x 355 screen-y 315))))))
               
         ;; old hacks left in
         (setf (mine-visible world) nil) 
         (setf (fortress-there world) nil)
         (setf (hit-hex world) nil)
         (setf (last-v world) nil)             
         
         ;; record the values from this game and set the list for the next game
         
         (push-last (getf data :raw-pnts) (first (game-stats world)))
         (push-last (getf data :bonus) (first (game-stats world)))
         (push-last (getf data :total-bonus) (first (game-stats world)))
         (push-last (append (getf data :base-stats) (getf data :event-stats) (getf data :input-stats)) (first (game-stats world)))
         
         (setf (last-time world) 0)
         (setf delay 1000)
         (push (list 0 0) (game-stats world))
         
         (schedule-event-relative delay 'incf-game-count :params (list world) :priority :max :time-in-ms t :maintenance t :output nil))
        
        (bonus ;; just quit the game since it got to the end
               ;; and there's nothing else that can be done
               ;; Not used in the current servers (2016), but left in 
               ;; anyways.
           
         (server-write-line world "quit"))
        
        (config ;; this is now handled in the connection function
                ;; Shouldn't see any while the game is running.
         
         (print-warning "Unexpected config data during the task: ~S" data))
        
        
        (game ;; This is where all the action happens...
           
         ;; If the display is being turned on/off between runs
         ;; send that notice now because it's only valid during
         ;; a game update not config or score
         
         (awhen (change-drawing-mode world)
                (server-write-line world "drawing ~d" it)
                (setf (change-drawing-mode world) nil))
           
         ;; compute how long to delay based on the time provided
         ;; unless there's an explicit delay given
         
         (let ((next-time (getf data :time)))
           (cond ((zerop delay)
                  (setf delay (- next-time (last-time world)))
                  (setf (last-time world) next-time))
                 (t
                  (setf (last-time world) (+ delay (last-time world))))))
         
         ;; Update all the visual features for the model
         
         (if (use-vision world)
             (progn 
               (awhen (getf data :collisions)
                      (when (find :fortress it)
                        (setf (f-hit-counter world) (1+ (max 3 (floor 100 delay)))))
                      
                      (when (find :small-hex it)
                        (setf (hit-hex world) (1+ (max 3 (floor 100 delay))))))
               
               (awhen (getf data :ship) 
                      
                      (if (getf it :alive)
                          (let ((ship-feat (gethash 'my-ship (all-locs world)))
                                (x (getf it :x))
                                (y (getf it :y))
                                (vel (getf it :speed))
                                (dist (getf it :distance-from-fortress))
                                (vx (getf it :vx))
                                (vy (getf it :vy))
                                (smallhex (aif (getf data :smallhex) it 40))
                                (last-vel nil))
                            
                            (setf (feature-now ship-feat) t)
                            
                            (aif (last-v world)
                                 (setf last-vel (sqrt (+ (* (car it) (car it)) (* (cdr it) (cdr it))))) 
                                 (setf last-vel 0))
                            
                            (setf (ship-pos world) (cons x y))
                            ;; Track velocity info and look for a bounce
                            ;; off of the inner hex (in games where that happens)
                            
                            (cond ((null (last-v world))
                                   (setf (last-v world) (cons vx vy)))
                                  ((and (= vx (car (last-v world))) (= vy (cdr (last-v world))))
                                   ;; do nothing
                                   )
                                  (t
                                   (when (and (numberp (hit-hex world)) (> (hit-hex world) 0))
                                     (decf (hit-hex world)))
                                   
                                   (when (and (numberp (hit-hex world)) (zerop (hit-hex world)))
                                     (setf (hit-hex world) nil))
                                   
                                   (let* ((od (atan (cdr (last-v world)) (car (last-v world))))
                                          (cd (atan vy vx))
                                          (dif (abs (- cd od))))
                                     (when (and (> dif 3) (< dif 3.3) (< dist (+ smallhex (* 2 (abs vel))))) ;; close to 180 shift and near inner hex
                                       
                                       ;; account for different delay values and get close to 100ms 
                                       ;; assume at least 3 since in the 30/sec they come in 33, 33, 34
                                       
                                       (setf (hit-hex world) (max 3 (floor 100 delay)))))
                                   
                                   (setf (car (last-v world)) vx)
                                   (setf (cdr (last-v world)) vy)))
                            
                            
                            (let ((mods (if (ship-feat world)
                                            (funcall (ship-feat world) it data)
                                          (let ((def (default-ship-features it data))
                                                (dist (getf it :distance-from-fortress)))
                                            (when (hit-hex world) 
                                              (push-last 'hex-hit def) 
                                              (push-last t def))
                                            
                                            (push-last 'speed-change def)
                                            (push-last (cond ((= vel last-vel) 0) ((< vel last-vel) -1) (t 1)) def)
                                            
                                            (push-last 'last-dist def)
                                            
                                            (if (last-dist world)
                                                (push-last (last-dist world) def)
                                              (push-last dist def))
                                            
                                            (setf (last-dist world) dist)
                                            def))))
                              
                              
                              ;; user set param to clear out old slots
                              (when (and (ship-feat world) (clear-ship-feat world) (feature-previous ship-feat))
                                (let ((current (do* ((r nil (cons s r))
                                                     (m mods (cddr m))
                                                     (s (car m) (car m)))
                                                    ((null s) r))))
                                  
                                  (dolist (s (chunk-filled-slots-list (feature-id ship-feat))) ;; assume the id is a chunk name
                                    (unless (find s current)
                                      (push-last s mods)
                                      (push-last nil mods)))))
                              
                              (if (feature-previous ship-feat)
                                  (modify-visicon-features (cons (feature-id ship-feat) (cddr mods)))
                                (setf (feature-id ship-feat) (car (add-visicon-features mods)))))
                            
                            (setf (ship-alive world) t))
                        
                        (let ((collisions (getf data :collisions))
                              (exp-feat (gethash 'ship-explosion (all-locs world))))
                          
                          (setf (feature-now exp-feat) t)
                          
                          ;; have to be careful now because the game keeps playing
                          ;; and sends multiple ticks while dead
                          ;; just count and update the feature on the first one
                          ;; assume that there is a ship feature...
                          ;; just change the value and kind on the ship feature
                          ;; and add a status slot
                          
                          (when (ship-alive world)
                            (incf (first (first (game-stats world))))
                            
                            (setf (ship-alive world) nil)
                            (setf (feature-id exp-feat)
                              (car (add-visicon-features (list 'isa 'ship-explosion 'screen-x (car (ship-pos world))
                                                               'screen-y (cdr (ship-pos world))
                                                               'status (cond ((find :small-hex collisions)
                                                                              'inner)
                                                                             ((find :big-hex collisions)
                                                                              'outer)
                                                                             ((find :shell collisions)
                                                                              'shell)
                                                                             (t
                                                                              'explosion))))))
                            
                            
                            ;; clear the special flags
                            (setf (hit-hex world) nil)
                            (setf (last-v world) nil)))))
               
               (awhen (getf data :shells)
                      
                      (let* (shell-x 
                             shell-y
                             (ship-vector (vector (car (ship-pos world)) (cdr (ship-pos world))))
                             best
                             (best-dist 10000)
                             (count 0))
                        
                        (dolist (s it)
                          (incf count)
                          (let* ((s-x (getf s :x))
                                 (s-y (getf s :y))
                                 (d (dist ship-vector (vector s-x s-y))))
                            
                            (when (< d best-dist)
                              (setf best s)
                              (setf best-dist d)
                              (setf shell-x s-x)
                              (setf shell-y s-y))))
                        
                        (let ((shell (gethash 'shell (all-locs world))))
                          (setf (feature-now shell) t)
                          (if (feature-previous shell)
                              (progn
                                (modify-visicon-features (list (feature-id shell)
                                                               'value count
                                                               'screen-x shell-x
                                                               'screen-y shell-y
                                                               'x shell-x
                                                               'y shell-y
                                                               'vx (getf best :vx)
                                                               'vy (getf best :vy)
                                                               'orientation (getf best :orientation))))
                            (setf (feature-id shell)
                              (car (add-visicon-features (list 'isa 'shell
                                                               'value count
                                                               'screen-x shell-x
                                                               'screen-y shell-y
                                                               'x shell-x
                                                               'y shell-y
                                                               'vx (getf best :vx)
                                                               'vy (getf best :vy)
                                                               'orientation (getf best :orientation)
                                                               ))))))))
               
               (awhen (getf data :fortress)
                      (when (getf it :alive)
                        (let ((feat (gethash 'fortress (all-locs world))))
                          (when (fort-exists world) ;; corrects for a bug that I don't want to
                            ;; track down on the python side
                            ;; If the ship is destroyed the fortress
                            ;; indicator gets sent even in the no fortress
                            ;; conditions (mines only since that's the only
                            ;; one where the ship can be destroyed)
                            
                            (setf (feature-now feat) t)
                            (unless (feature-previous feat)
                              (setf (feature-id feat)
                                (car (add-visicon-features (list 'isa 'fortress 'kind 'fortress 'screen-x (elt (fort-pos world) 0) 'screen-y (elt (fort-pos world) 1))))))
                            
                            (setf (fortress-killed world) nil)
                            (setf (fortress-there world) t)))))
               
               (awhen (getf data :missiles)
                      (let* (missile-x 
                             missile-y
                             (fort (fort-pos world))
                             best
                             (best-dist 10000)
                             (count 0))
                        
                        (dolist (m it)
                          (incf count)
                          (let* ((m-x (getf m :x))
                                 (m-y (getf m :y))
                                 (d (dist fort (vector m-x m-y))))
                            (when (< d best-dist)
                              (setf best m)
                              (setf best-dist d)
                              (setf missile-x m-x)
                              (setf missile-y m-y))))
                        
                        (let ((missile (gethash 'missile (all-locs world))))
                          (setf (feature-now missile) t)
                          (if (feature-previous missile)
                              (modify-visicon-features (list (feature-id missile)
                                                             'x missile-x 'y missile-y
                                                             'screen-x missile-x 'screen-y missile-y
                                                             'value count))
                            (setf (feature-id missile)
                              (car (add-visicon-features (list 'isa 'missile
                                                               'value count
                                                               'screen-x missile-x
                                                               'screen-y missile-y
                                                               'x missile-x
                                                               'y missile-y
                                                               'vx (getf best :vx)
                                                               'vy (getf best :vy)
                                                               'orientation (getf best :orientation)))))))))
               
               (awhen (getf data :vlner)
                      (let ((f (gethash 'vlner (all-locs world))))
                        (setf (feature-now f) t)
                        (if (feature-previous f)
                            (modify-visicon-features (list (feature-id f)
                                                           'value it))
                          (setf (feature-id f)
                            (car (add-visicon-features (list 'isa 'labeled-text
                                                             'screen-x 465
                                                             'screen-y 700
                                                             'kind 'text
                                                             'label 'vlner
                                                             'value it)))))))
               
               (awhen (getf data :pnts)
                      (let ((f (gethash 'pnts (all-locs world))))
                        (setf (feature-now f) t)
                        (if (feature-previous f)
                            (modify-visicon-features (list (feature-id f)
                                                           'value it))
                          (setf (feature-id f)
                            (car (add-visicon-features (list 'isa 'labeled-text
                                                             'screen-x 245
                                                             'screen-y 700
                                                             'kind 'text
                                                             'label 'points
                                                             'value it)))))))
               
               (awhen (getf data :iff)
                      (let ((f (gethash 'pnts (all-locs world))))
                        (setf (feature-now f) t)
                        (if (feature-previous f)
                            (modify-visicon-features (list (feature-id f)
                                                           'value it))
                          (setf (feature-id f)
                            (car (add-visicon-features (list 'isa 'labeled-text
                                                             'screen-x 600
                                                             'screen-y 700
                                                             'kind 'text
                                                             'label 'iff
                                                             'value it)))))))
               
               (awhen (getf data :bonus)
                      (let ((f (gethash 'pnts (all-locs world))))
                        (setf (feature-now f) t)
                        (if (feature-previous f)
                            (modify-visicon-features (list (feature-id f)
                                                           'value it))
                          (setf (feature-id f)
                            (car (add-visicon-features (list 'isa 'bonus
                                                             'screen-x 355
                                                             'screen-y 420
                                                             'kind 'bonus
                                                             'label 'bonus
                                                             'value it)))))))
               
               
               
               
               (awhen (getf data :mine)
                      #|
                 just ignore this for now because we don't have
                 mines and the feature details used for the original model
                 should probably be reworked...

                (mod-chunk-fct 'mine-loc (list 'visible t 'kind 'mine))
                
                ;; record the data for shooting a mine
                
                (let* ((x (getf it :x))
                       (y (getf it :y))
                       (o (chunk-slot-value my-ship orientation))
                       (sx (chunk-slot-value my-ship x))
                       (sy (chunk-slot-value my-ship y))
                       (ma (rad->deg (atan (- sy y) (- x sx))))
                       (a (- o ma))
                       (side 'ahead)
                       (dist (dist (list x y) (list sx sy)))
                       (hit-dist (abs (* (sin (deg->rad a)) dist)))
                       (fa (chunk-slot-value my-ship angle))
                       (fd (chunk-slot-value my-ship dist))
                       
                       (hit-f-dist (abs (* (sin (deg->rad fa)) fd))))
           
                  (when (> a 180)
                    (setf a (- a 360)))
               
                  (when (< a -180)
                    (setf a (+ a 360)))
                  
                  (unless (<= -90 a 90)
                    (setf side 'behind))
               
                  (mod-chunk-fct 'my-ship (list 'mine-side side))
               
                  (mod-chunk-fct 'mine (list 'x x 'y y))
                  (mod-chunk-fct 'mine-loc (list 'screen-x x 'screen-y y))
               
                  (mod-chunk-fct 'my-ship (list 'mine-dist dist 'shoot-at-mine (<= hit-dist 20) 'hit-fortress-first (and (<= hit-f-dist 18) (< fd dist))))
               
                  (setf (mine-visible world) 12))
                  |#                
                      
                      )) 
           
           (progn ;; still handle some of the internal values
             (awhen (getf data :fortress)
                    (if (getf it :alive)
                        (when (fort-exists world) 
                          (setf (fortress-killed world) nil)
                          (setf (fortress-there world) t))
                      (setf (fortress-there world) :dead)))
             
             (awhen (getf data :ship) 
                    
                    (if (getf it :alive)
                        (setf (ship-alive world) t)
                      
                      (when (ship-alive world)
                        (incf (first (first (game-stats world))))
                        
                        (setf (ship-alive world) nil))))
             
             )))
        
        (t (print-warning "unexpected screen-type: ~s" data)))
    
    
      ;; Do some special processing for some of the visual information
      
      ;; hit hex is a bounce detector which keeps this set for a few
      ;; ticks so the model can detect it, but currently the ship doesn't
      ;; bounce so this being set just happens when the ship explodes
      
      ;; already set when processing ship now
      ;; (when (hit-hex world)
      ;;  (mod-chunk-fct 'my-ship (list 'hex-hit t)))
      
      ;; a fortress-hit stays "visible" for roughly ~100ms (at least 3 ticks)
      ;; may also want to use an audio cue for that too since
      ;; the game does make a sound, but for now just keeping
      ;; it visual
      
      (when (use-vision world)
        (when (numberp (f-hit-counter world))
          (decf (f-hit-counter world))
          
          (if (or (zerop (f-hit-counter world)) (null (fortress-there world)))
              (setf (f-hit-counter world) nil)
            (let ((f (gethash 'fortress-hit (all-locs world))))
              (setf (feature-now f) t)
              (unless (feature-previous f)
                (setf (feature-id f)
                  (car (add-visicon-features (list 'isa 'fortress-hit
                                                   'screen-x (elt (fort-pos world) 0) 
                                                   'screen-y (elt (fort-pos world) 1) 
                                                   'kind 'fortress-hit)))))))))
      
      
      ;; there should be fortress but it's not visible
      ;; originally there wasn't a fortress feat if it was
      ;; dead, but that may have changed now.
      ;; To be safe still handle this separately and
      ;; not just change the kind on the fortress feature.
      
      (if (use-vision world)
          (let ((feat (gethash 'fortress (all-locs world))))
            (when (and (fortress-there world) (not (feature-now feat)))
              
              ;; count the kill only once
              (unless (fortress-killed world)
                (incf (second (first (game-stats world))))
                (setf (fortress-killed world) t))
              
              (let ((explosion (gethash 'fortress-explosion (all-locs world))))
                (setf (feature-now explosion) t)
                (unless (feature-previous explosion)
                  (setf (feature-id explosion)
                    (car (add-visicon-features (list 'isa 'fortress-explosion 
                                                     'screen-x (elt (fort-pos world) 0) 
                                                     'screen-y (elt (fort-pos world) 1)))))))))
        (progn
          (if (eq (fortress-there world) :dead)
              (progn
                (setf (fortress-there world) t)
                (unless (fortress-killed world)
                  (incf (second (first (game-stats world))))
                  (setf (fortress-killed world) t)))
            ;; do I need an else? or does that cover the flags
            )))
    
      
      ;; there should be a mine but its loc isn't visible
      ;; then flag it as a dead-mine
      
      #| ignoring mines for now
      (when (and (mine-visible world)
                 (null (fast-chunk-slot-value-fct 'mine-loc 'visible))
                 (not (zerop (mine-visible world))))
        (mod-chunk-fct 'mine-loc (list 'visible t 'kind 'dead-mine))
        (decf (mine-visible world)))
      |#
      ;; Remove everything that was there but isn't now
      
      (when (use-vision world)
        (maphash (lambda (key feat)
                   (declare (ignore key))
                   (when (and (null (feature-now feat))
                              (feature-previous feat)
                              (feature-id feat))
                     (delete-visicon-features (feature-id feat))
                     (setf (feature-id feat) nil)))
                 (all-locs world)))
        
      
      ;; Schedule the next update based on the needed delay
      
      (schedule-event-relative delay 'update-world :destination :sf :module :sf :maintenance (use-vision world) :time-in-ms t))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some function to handle trig info (some of these come over directly now so 
;;; and don't need to be computed on the model side).

(defun vdir (x y vx vy)
  (let* ((dir (atan vy vx))
         (actual (to-fortress-orient x y))
         (diff (rad->deg (- dir actual))))
    (if (> diff 180)
        (- diff 360)
      (if (< diff -180)
          (+ diff 360)
        diff))))


(defun angle-to-fortress (x y orient)
  (let* ((actual (rad->deg (to-fortress-orient x y)))
         (diff (- orient actual)))
	(if (> diff 180)
        (- diff 360)
      (if (< diff -180)
          (+ diff 360)
        diff))))

(defun to-fortress-orient (x y)
  (let ((dx (- 355 x))
        (dy (- y 315)))
    (atan dy dx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The module to provide the interface, parameters, etc

(defun create-sf-module (name)
  (declare (ignore name))
  
  ;; specify chunks for the features to avoid warnings
  
  (define-chunks ship explosion fortress fortress-explosion bonus
    vlner pnts fortress-hit shell missile trial-end foe
    condition iff mine debris inner outer points)
  
  (make-instance 'sf-world))

(defun pre-reset-sf-module (world)
  
   ;; create custom chunk-types for the features and objects

  (chunk-type (sf-visual-location (:include visual-location)) visible)
  (chunk-type (sf-object (:include visual-object)(:include sf-visual-location)) label)
  
  (chunk-type (ship (:include sf-object)) (ship t) (label ship) 
              x y angle vel dist vdir hex-dist orientation 
              last-dist hex-hit mine-side mine-dist shoot-at-mine 
              hit-fortress-first vx vy speed-change travel-time-to-bighex 
              travel-time-to-smallhex dist-to-bighex dist-to-smallhex 
              travel-dist-to-bighex travel-dist-to-smallhex (kind ship))
  (chunk-type (ship-explosion (:include sf-object)) (label explosion)(value explosion))
  (chunk-type (mine (:include sf-object)) x y (label mine)(kind mine))
  (chunk-type (fortress (:include sf-object)) angle (label fortress)(kind fortress))
  (chunk-type (fortress-explosion (:include sf-object)) (label fortress-explosion)(kind fortress-explosion))
  
  (chunk-type (bonus (:include sf-object)) (bonus t) (label bonus)(kind bonus))
  (chunk-type (trial-end (:include sf-object)) (trial-end t) (kind new-trial-end) (label trial-end))
  (chunk-type (projectile (:include sf-object)) x y vx vy orientation count)
  (chunk-type (missile (:include projectile)) (label missile)(kind missile))
  (chunk-type (shell (:include projectile)) (label shell) (kind shell))
  (chunk-type (labeled-text (:include sf-object)(:include text)))
  (chunk-type (fortress-hit (:include sf-object)) (label fortress-hit)(kind fortress-hit))
  
  ;; some types for the calculations that 'cheat' through an imaginal action
  
  (chunk-type mine-heading action finger delay ticks dist hit-dist)
  (chunk-type reorbit turn thrust finger)

  ;; Kill any previous connection
  
  (when (server world)
    (sfapi-disconnect (server world))
    (setf (server world) nil))
  
  ;; clear out the slots as needed
 
  (setf (game-state world) 'disconnected)
  (clrhash (all-locs world))
  
  (setf (data-record world) nil)
  
  (setf (f-hit-counter world) nil)
  (setf (hit-hex world) nil)
  (setf (last-v world) nil)
  (setf (mine-visible world) nil)
  (setf (ship-pos world) nil)
  
  (setf (responses world) nil)
  (setf (auto-response world) nil)
  
  ;; hack to make the fortress always visible
  ;; since that's the case for this task
  ;; and there are no condition screens
  
  (setf (fort-exists world) t)
  
  (setf (ship-alive world) nil)
  
  (setf (fortress-there world) nil)
  (setf (last-time world) 0)
  
  ;; track deaths and kills per game
  (setf (game-stats world) (list (list 0 0)))
  
  ;; name for the data file
  (setf (name world) nil)
  
  ;; track complete games
  (setf (game-count world) 0)
  (setf (game-limit world) -1)
  (setf (change-drawing-mode world) nil))

  
(defun post-reset-sf-module (world)
  
 
  ;; create all the possible features up front and then
  ;; add/modify/delete as needed
  
  (when (use-vision world)
    (mapcar (lambda (x) (setf (gethash x (all-locs world)) (make-feature)))
      '(foe1 foe2 foe3 cond1 cond2 iff mine my-ship fortress bonus vlner pnts fortress-hit shell missile trial-end fortress-explosion ship-explosion)))
  
  ;; Set :tracking-clear to nil so it re-encodes explosions
  ;; probably not necessary now that the ship and fortress features are
  ;; being modified to be explosions, but just to be safe...
  
  (sgp :tracking-clear nil)
  
  (when (key-handlers world)
    (remove-act-r-command-monitor "output-key" "sf-key-down")
    (remove-act-r-command-monitor "release-key" "sf-key-up")
    (setf (key-handlers world) nil))
  
  ;; clear the current condition
  
  (setf (game-condition world) nil))


(defun delete-sf-module (world)
  ;; Kill any open connections
  
  (when (server world)
    (sfapi-disconnect (server world))
    (setf (server world) nil)))

;; respond to the game buffer queries

(defun sf-query (world buffer-name slot value)
  (case buffer-name
    
    (game (case slot
            (state
             (case value
               (busy nil)
               (free t)
               (error nil)
               (t (print-warning "Unknown state query ~S to game buffer" value)
                  nil)))
            (game-state
                (eq (game-state world) value))
            (t (print-warning "Unknown query ~S ~S to the game buffer" slot value))))))



;; custom function for printing the buffer-status of the game buffer

(defun print-sf-game-state ()
  (command-output "  game-state connecting  : ~S"
                  (query-buffer 'game
                                '(game-state connecting)))
  (command-output "  game-state connected   : ~S"
                  (query-buffer 'game
                                '(game-state connected)))
  (command-output "  game-state disconnected: ~S"
                  (query-buffer 'game
                                '(game-state disconnected))))


;; function to handle the setting and getting of the module's parameters

(defun sf-params (world param)
  (cond ((consp param)
         
          (case (car param)
            (:sf-host (setf (sf-host world) (cdr param)))
            (:sf-port (setf (sf-port world) (cdr param)))
            (:sf-server-type (setf (sf-server-type world) (cdr param)))
            (:log-sf-data-in (setf (collect-incoming world) (cdr param)))
            (:log-sf-data-out (setf (record-outgoing world) (cdr param)))
            (:sf-fixation-hook (setf (fix-hook world) (cdr param)))
            (:sf-start-hook (setf (start-hook world) (cdr param)))
            (:sf-end-hook (setf (end-hook world) (cdr param)))
            (:sf-data-hook (setf (data-hook world) (cdr param)))
            (:ship-feature (setf (ship-feat world) (cdr param)))
            (:clear-ship-feat (setf (clear-ship-feat world) (cdr param)))
            (:use-vision (setf (use-vision world) (cdr param)))))
        (t 
         (case param
           (:sf-host (sf-host world))
           (:sf-port (sf-port world))
           (:sf-server-type (sf-server-type world))
           (:log-sf-data-in (collect-incoming world))
           (:log-sf-data-out (record-outgoing world))
           (:sf-fixation-hook (fix-hook world))
           (:sf-start-hook (start-hook world))
           (:sf-end-hook (end-hook world))
           (:sf-data-hook (data-hook world))
           (:ship-feature (ship-feat world))
           (:clear-ship-feat (clear-ship-feat world))
           (:use-vision (use-vision world))))))


;; define the module itself

(define-module-fct :sf '((game nil nil (game-state) print-sf-game-state)) 
  (list
   (define-parameter :sf-host :valid-test #'stringp :default-value "127.0.0.1"
     :warning "a string" :documentation "IP address or host name of SF server")
   (define-parameter :sf-port :valid-test 'integerp  :default-value 3000
     :warning "an integer" :documentation "Gamebots port of the server for connecting")
   (define-parameter :sf-server-type :valid-test (lambda (x) (find x '(sfapi-tcp-client sfapi-external-process-client)))
                                     :default-value 'sfapi-tcp-client
     :warning "sfapi-tcp-client or sfapi-external-process-client" :documentation "server type to use.")
   (define-parameter :log-sf-data-in :valid-test #'tornil :default-value nil
     :warning "T or nil" :documentation "Record all incoming SF data")
   (define-parameter :log-sf-data-out :valid-test #'tornil :default-value nil
     :warning "T or nil" :documentation "Record all commands sent to SF")
   (define-parameter :sf-fixation-hook :valid-test #'fctornil 
     :default-value nil
     :warning "a function or nil" :documentation "Middle of fixation hook")
   (define-parameter :sf-start-hook :valid-test #'fctornil 
     :default-value nil
     :warning "a function or nil" :documentation "Game start hook")
   (define-parameter :sf-end-hook :valid-test #'fctornil 
     :default-value nil
     :warning "a function or nil" :documentation "Game end hook")
   
   (define-parameter :sf-data-hook :valid-test #'fctornil 
     :default-value nil
     :warning "a function or nil" :documentation "Called with each data line received from the game interface")
   (define-parameter :clear-ship-feat :valid-test #'tornil :default-value t
     :warning "T or nil" :documentation "Whether or not to clear all the slots from the ship chunk before applying the ship-feature hook result")
   (define-parameter :ship-feature :valid-test #'fctornil 
     :default-value nil
     :warning "a function or nil" :documentation "Called to get the features for the ship object")
   (define-parameter :use-vision :valid-test #'tornil 
     :default-value nil
     :warning "t or nil" :documentation "Indicate whether visual features should be created"))
  
  :version "8.0"
  :documentation "Module to allow ACT-R 7.x to connect to SpaceFortress with our custom server interface"
  :creation 'create-sf-module
  :reset (list 'pre-reset-sf-module nil 'post-reset-sf-module)
  :delete 'delete-sf-module
  :query 'sf-query
  :params 'sf-params)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Handle all the config info. for the current game servers when a new session gets
;;; started with one of the running functions
;;;
;;; - read the first config line
;;; - send draw and id settings verifying that the result is t for each
;;;
;;; If anything doesn't work out punt and close the connection

(defun same-id-test (requested returned)
  (unless (stringp requested)
    (setf requested (princ-to-string requested)))
  (string-equal requested returned))

(define-condition config-error (error)
  ((msg :initform nil :initarg :msg :accessor config-error-msg))
  (:report (lambda (condition stream)
             (format stream "error during config: ~A" (config-error-msg condition)))))

(defun config-cleanup-after-error (module)
  (when (server module)
    (sfapi-disconnect (server module))
    (setf (server module) nil))
  (setf (game-state module) 'disconnected))

(defun config-parse-data-line (module message)
  (multiple-value-bind (data err)
      (ignore-errors (read-from-string (get-line-of-data-from-game module)))
    (if (subtypep (type-of err) 'condition)
        (error 'config-error :msg message)
        (progn
          (when (collect-incoming module)
            (push (list :in (mp-time) (get-internal-real-time) data) (data-record module)))
          data))))

(defun config-set-parameter (module expected-screen key value)
  (server-write-line module "~A ~A" key value)
  (let ((data (config-parse-data-line module (format nil "Failed reading response to ~A setting" key))))
    (unless (and (eq expected-screen (read-from-string (getf data :screen-type)))
                 (getf data :result))
      (error 'config-error :msg (format nil "Did not get a true config result from changing ~A -> ~A: ~S" key value data)))
    data))

(defun config-set-settings (module expected-screen &key draw condition log-format config game-number seed extra)
  (config-set-parameter module expected-screen "drawing" (if draw 1 0))
  (when condition (config-set-parameter module expected-screen "condition" condition))
  (when log-format (config-set-parameter module expected-screen "logformat" log-format))
  (when game-number (config-set-parameter module expected-screen "gnum" game-number))
  (when seed (config-set-parameter module expected-screen "seed" seed))
  (dolist (pair config)
    (config-set-parameter module expected-screen (format nil "config ~A" (first pair)) (second pair)))
  (dolist (pair extra)
    (config-set-parameter module expected-screen (format nil "extra ~A" (first pair)) (second pair))))

(defun connect-to-game (module &key draw name condition log-format config game-number seed extra)
  (when (not (eq (game-state module) 'disconnected))
    (print-warning "Model is already connected cannot connect.")
    (return-from connect-to-game))
  
  (unless (key-handlers module)
    (monitor-act-r-command "output-key" "sf-key-down")
    (monitor-act-r-command "release-key" "sf-key-up")
    
    
    (install-device '("motor" "keyboard"))
    
    (setf (key-handlers module) t))

  (handler-case
      (progn
        (setf (server module) (make-instance (sf-server-type module)))
        (sfapi-connect (server module))
        (when (not (server module))
          (error 'config-error :msg "Could not open server connection."))
        (let ((data (config-parse-data-line module "Failed to read initial config")))
          (setf (server-current-screen module) (read-from-string (getf data :screen-type)))
          (unless (eq 'config (server-current-screen module))
            (error 'config-error :msg (format nil "Expected a config line but got ~s" data)))

          (config-set-settings module 'config
                               :draw draw
                               :condition condition
                               :log-format log-format
                               :config config
                               :game-number game-number
                               :seed seed
                               :extra extra)

          (let ((n (if name
                       name
                       (if (and (boundp '*cluster-run-start*) (boundp '*cluster-run-id*))
                           (progn
                             (format nil "model_~d_~d" (symbol-value '*cluster-run-start*) (symbol-value '*cluster-run-id*)))
                           (let (number)
                             (if (probe-file (translate-logical-pathname "SF-DATA:sf-model-number.txt"))
                                 (with-open-file (f (translate-logical-pathname "SF-DATA:sf-model-number.txt") :direction :input)
                                   (setf number (ignore-errors (read f nil :done))))
                                 (setf number -1))
                             (if (numberp number)
                                 (incf number)
                                 (setf number 0))
                           
                             (with-open-file (f (translate-logical-pathname "SF-DATA:sf-model-number.txt") :direction :output :if-exists :supersede :if-does-not-exist :create)
                               (write number :stream f))
                             (format nil "model_~d" number))))))
                
            (setf (name module) n)
                
            (config-set-parameter module 'config "id" n)
                
            (setf (game-state module) 'connected))))
    (config-error (c)
      (print-warning (config-error-msg c))
      (config-cleanup-after-error module)
      nil)))

(defun connect-to-game-in-progress (module &key draw name condition log-format config game-number seed extra)
  (when (eq (game-state module) 'disconnected)
    (print-warning "Model must already be connected.")
    (return-from connect-to-game-in-progress))
  (handler-case
      (progn
        (when (eq (server-current-screen module) 'game)
          (server-write-line module "abort")
          (let ((data (config-parse-data-line module "Failed to abort game")))
            (setf (server-current-screen module) (read-from-string (getf data :screen-type)))))
        (unless (eq 'score (server-current-screen module))
          (error 'config-error :msg "Expected score screen.")
          (return-from connect-to-game-in-progress))
        (config-set-settings module 'score
                             :draw draw
                             :condition condition
                             :log-format log-format
                             :config config
                             :game-number game-number
                             :seed seed
                             :extra extra)
        (when name
          (setf (name module) name)
          (config-set-parameter module 'score "id" name))
        t)
    (config-error (c)
      (print-warning (config-error-msg c))
      (config-cleanup-after-error module)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions for actually playing the game -- call these to run the model.
;;;
;;; The game can be run for a specific time in seconds using the play-sf function
;;; or a fixed number of games using the play-sf-games function.  Each of those
;;; has one required parameter for the indicated value (time or games).
;;;
;;; They also have 4 keyword parameters:
;;;
;;; :speed should be a number and indicates how fast to run the game relative
;;;        to real-time.  The default is 1 which means at real time.  A bigger 
;;;        value runs the model faster than real time and a smaller value slower.
;;;
;;; :cont whether the current run should be continuted.  If this is specified
;;;       as t then it does not reconnect the model before starting running
;;;       and if it is nil then it resets the model and starts a new session.
;;;       The default is nil.
;;;
;;; :draw can be t, nil, or :debug.  Indicates whether the game server displays
;;;       the game as the model plays.  A value of t means show everything and nil
;;;       means show nothing.  For some versions of the server :debug causes a
;;;       very limited display (sufficient to see that something is happening and
;;;       it's not hung), but not all support that mode in which case it will
;;;       show everything like t does.  The default value is t.
;;;
;;; :name should be a symbol, string, or number which is used to create a directory
;;;       in the game's data folder for the model's game data files.  If it is not
;;;       provided then the name will be "model_#" where # is one more than the 
;;;       number read from the sf-model-number.txt file found in the same directory
;;;       as the one from which this file was loaded or the number 0 if that file
;;;       is not found.  When it generates a number to use it will write that number
;;;       to the sf-model-number.txt file (creating the file if it does not exist).
;;;
;;; :condition the string which names the game condition to play.
;;;
;;; :log-format not sure...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Game now runs in real-time mode to allow for speed control (doesn't use a custom
;;; clock anymore to simplify things).

(defun play-sf (time &key (speed 1) (cont nil) (draw t) name condition log-format config game-number seed)
  (let ((module (get-module :sf)))
    (if module
        (progn
          (if cont
              (if (or (null condition) (equalp condition (game-condition module)))
                  (progn
                    (setf (change-drawing-mode module) (if draw (if (eq draw :debug) 1 2) 0))
                    (run-full-time time speed))
                (progn
                  (pre-reset-sf-module module)
                  (when (connect-to-game module :draw draw :name name :condition condition :log-format log-format :config config :game-number game-number :seed seed)
                    (setf (game-condition module) condition)
                    (let* ((events (meta-p-events (current-mp)))
                           (event (find 'update-world events :key 'act-r-event-action)))
                      (when event
                        (delete-event (act-r-event-num event)))
                      (schedule-event-now 'update-world :destination :sf :module :sf :priority :max))
                    (run-full-time time speed))))
            (progn
              (reset)
              
              (when (connect-to-game module :draw draw :name name :condition condition :log-format log-format :config config :game-number game-number :seed seed)
                (setf (game-condition module) condition)
                (schedule-event 0 'update-world :destination :sf :module :sf :priority :max :time-in-ms t)
                (run-full-time time speed)))))
      (print-warning "Cannot play space fortress"))))


(defun play-sf-games (games &key (speed 1) (cont nil) (draw t) name condition log-format config game-number seed extra)
  (let ((module (get-module :sf)))
  (defun check-game-count (x) (declare (ignore x)) (>= (game-count module) (game-limit module)))
    (cond
      ((not module)
       (print-warning "Cannot play space fortress"))
      (cont
       (setf (game-count module) 0)
       (setf (game-limit module) games)
       (when (connect-to-game-in-progress module :draw draw :name name :condition condition :log-format log-format :config config :game-number game-number :seed seed :extra extra)
         (let* ((events (meta-p-events (current-mp)))
                (event (find 'update-world events :key 'act-r-event-action)))
           (when event
             (delete-event (act-r-event-num event)))
           (schedule-event-now 'update-world :destination :sf :module :sf :priority :max))
         (run-until-condition 'check-game-count speed)))
      (t
       (reset)
       (setf (game-count module) 0)
       (setf (game-limit module) games)
       
       (when (connect-to-game module :draw draw :name name :condition condition :log-format log-format :config config :game-number game-number :seed seed :extra extra)
         (setf (game-condition module) condition)
         (schedule-event 0 'update-world :destination :sf :module :sf :priority :max :time-in-ms t)
         (run-until-condition 'check-game-count speed))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load all the other modules and code that this depends on once and create a logical
;;; directory called SF-DATA pointing to the directory this came from.
;;;

(require-extra "extended-motor-actions")
(require-extra "tracker")

(unless (boundp '*loaded-modules*)
  (defparameter *loaded-modules* t)
  (compile-and-load "ACT-R:space-fortress-modules;visual-search-buffer.lisp")
  (compile-and-load "ACT-R:space-fortress-modules;analyze-position.lisp")
  (compile-and-load "ACT-R:space-fortress-modules;attend-and-track.lisp")
  
  (setf (logical-pathname-translations "SF-DATA")
    `(("**;*.*" ,(namestring (merge-pathnames "**/*.*" *load-truename*))))))
  

#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
