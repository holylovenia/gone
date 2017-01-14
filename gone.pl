/* Dynamic Declarations */
:- dynamic(location/2).
:- dynamic(here/1).
:- dynamic(have/1).
:- dynamic(cold/1).
:- dynamic(warm/1).
:- dynamic(attribute/1).
:- dynamic(atime/2).
:- dynamic(step/1).
:- dynamic(stept/1).
:- dynamic(money/1).


/* START */

start :-
		init_dynamic_facts,
        write('         _______                                      '), nl,
        write('        /  _____\\                                     '), nl,
        write('       /  //                                          '), nl,
        write('       |  || ___  ______  ________  _______           '), nl,
        write('       |  ||/__ \\/  __  \\/  ___  ||/ ____ ||          '), nl,
        write('       |  ||  | || || | || ||  | || ______//          '), nl,
        write('       |  \\\\__/ || ||_| || ||  | || \\_____    _       '), nl,
        write('        \\_______/\\______/|_||  | ||\\______// /_/      '), nl, nl, nl,
		write('Your slumber is yet again disturbed by the obnoxiously'), nl,
		write('loud alarm clock on the dresser across the room.'), nl, nl,
		write('It was so loud this time that the urge to turn it off'), nl,
		write('is far greater than your will to sleep longer. With great'), nl,
		write('plain white ceiling.'), nl, nl,
		write('Unable to bear the loud noise any longer, you tried to get'), nl,
		write('up and fail miserably.'), nl, nl,
		write('Your body refused to get up and fell back on the bed.'), nl,
		write('The alarm clock is still ringing, loudly.'), nl, nl,
		write('Press "enter" key to continue'), nl,
		get0(_),
		write('"instructions" if you need help.'), nl,
		look, plusoneminute(_,_), nl, !,
		command_loop.

credits :- write('Holy(ry) Clinton Team'), nl, nl,
		write('13515035 - Oktavianus Handika'), nl,
		write('13515065 - Felix Limanta'), nl,
		write('13515077 - Rionaldi Chandraseta'), nl,
		write('13515113 - Holy Lovenia'), nl, nl,
		write('13515143 - Agus Gunawan'), nl.
		

command_loop:-
  repeat,
  catch((
	 	write('> '),
  		read(X),
  		do(X),
  		((lose, step(6)); win; X == quit)) ,
 	 	error(syntax_error(_),_), 
  		(write('Your input is invalid. "instructions" if you need help.'), 
		nl, fail, !) ).

do(inspect(X)):- inspect(X),!.
do(strangle(X)):- strangle(X),!.
do(use(X)):- use(X),!.
do(check(X)):- check(X),!.
do(move(X)):- move(X),!.
do(take(X)):- take(X),!.
do(talk(X)) :- talk(X),!.
do(drop(X)):- drop(X),!.
do(inventory):- inventory,!.
do(drive(X)):- drive(X),!.
do(eat(X)):- eat(X),!.
do(look):- look,!.
do(instructions):- instructions,!.
do(stat) :- stat, !.
do(n):- n,!.
do(s):- s,!.
do(w):- w,!.
do(e):- e,!.
do(u):- u,!.
do(d):- d,!.
do(quit) :- quit, !.
do(save(X)) :- save(X),!.
do(load(X)) :- loads(X), !.
do(howmuch) :- howmuch, !.
do(credits) :- credits, !.
do(_) :- respond.

win :- here('out of town'),
  write('You had escaped from the town. Another successful day on the job.'),nl,nl,
  inventory,!,nl,
  write('Value of stolen goods: '), howmuch, nl,
  write('As you drive, you hoped that the police would not know about the corpse for about three days. You would have been long gone after that..'), nl,
  write('The End'), nl, abort.

quit :-
	write('You''ve quitted your game'),nl,abort,!.

respond :- write('"instructions" if you need help.'), nl,!,fail.

/* Stat */
stat :- attribute(X),
		tab(2), write(X), nl, fail;true.

/* Step */
plusonestep(X,Y) :- Y is X + 1.

/* Time */
changem(M,H,NM,NH) :- M > 59,
	NM is M - 60,
	NH is H + 1.
changeh(M,H,NM,NH) :- H > 23,
	NH is H - 24,
	NM is M.
changemh(M,H,NM,NH) :- M > 59, H > 23,
	NH is H - 23,
	NM is M - 60.
calculate_next(Minutes,MI,NextM) :-
	NextM is MI+Minutes.

plusoneminute(MM,HH) :-
	atime(HH,MM),
	calculate_next(MM,1,FM),
	timebasis(FM,HH,NM,NH), retract(atime(HH,MM)),
	assertz(atime(NH,NM)).

plusPminute(MM,HH,MI) :-
	atime(HH,MM),
	calculate_next(MM,MI,FM),
	timebasis(FM,HH,NM,NH), retract(atime(HH,MM)),
	assertz(atime(NH,NM)).

timebasis(M,H,NM,NH) :-
	((changemh(M,H,NM,NH)); (changem(M,H,NM,NH)); (changeh(M,H,NM,NH)); (NM is M, NH is H)), asserta(atime(NH,NM)), nl,
	write('Time now: '), ((NH < 10, write('0'),write(NH)) ; write(NH)), write('.'), ((NM < 10, write('0'), write(NM)) ; write(NM)) , nl, !.

/* Rooms */
room(bathroom).
room(bedroom).
room('living room').
room(kitchen).
room('front yard').
room('director''s cabin').
room(office).
room(reception).
room(intersection).
room('out of town').


/* Buildings */
house([bathroom, bedroom, 'living room', kitchen, 'front yard']).
workplace(['director''s cabin', office, reception]).
street([intersection,'out of town']).

/* YOUR First Position */


/* Directions */
west(bathroom, bedroom).
west('living room', kitchen).
north(bedroom, 'living room').
north('living room', 'front yard').
north('director''s cabin', office).
east(kitchen, 'living room').
east(bedroom, bathroom).
south('living room', bedroom).
south('front yard', 'living room').
south(office, 'director''s cabin').
up(office, reception).
down(reception, office).
way('out of town', intersection).
way(intersection, 'out of town').
way(intersection, 'front yard').
way('front yard', intersection).
way(intersection, reception).
way(reception, intersection).

/* Objects */
object(car,large,0).
object(tv,large,0).
object(alarm,small,0).
object(shower,large,0).
object(microwave,large,0).
object(bed,large,0).
object('car key',small,0).
object('gold ring',small,5000).
object(safe,large,0).
object(gold,small,100000).
object(money,small,10000).
object(computer,large,0).
object(drawer,large,0).
object(painting,large,0).
object(corpse,large,0).
object(fridge,large,0).
object(leftover,small,0).
object(soup,small,0).
object(clothes,small,0).
object(card,small,0).
object('dog food', small,0).
object(paper, small,0).
object(flowerbed, large,0).
object(toilet, large,0).
object(sink, large,0).
object(bench,large,0).
object(plant,large,0).
object(desk,large,0).
object('sticky notes',small,0).
object(document,small,0).
object('photocopy machine',large,0).
object('coffee maker',large,0).
object('water dispenser',large,0).
object(endtable,large,0).
object(counter,large,0).
object(stove,large,0).
object(window,large,0).
object(sofa,large,0).
object(note,small,0).
object(knife,small,0).

/* NPC */
npc(receptionist).
npc(dog).
npc('co-workers').
npc(director).


/* Objects' Position */
init_dynamic_facts :-
	delete_dynamic_facts,
	asserta(location(car, 'front yard')),
	asserta(location(card, 'living room')),
	asserta(location(tv, 'living room')),
	asserta(location(alarm, bedroom)),
	asserta(location(shower, bathroom)),
	asserta(location(microwave, kitchen)),
	asserta(location(bed, bedroom)),
	asserta(location(note, bedroom)),
	asserta(location(drawer, bedroom)),
	asserta(location('car key', drawer)),
	asserta(location(safe, painting)),
	asserta(location(painting, 'living room')),
	asserta(location(money, safe)),
	asserta(location(gold, safe)),
	asserta(location('gold ring', corpse)),
	asserta(location(computer, office)),
	asserta(location(corpse, bed)),
	asserta(location(leftover, fridge)),
	asserta(location(soup, fridge)),
	asserta(location(fridge, kitchen)),
	asserta(location(receptionist, reception)),
	asserta(location(director, 'director''s cabin')),
	asserta(location(dog, 'front yard')),
	asserta(location('dog food', kitchen)),
	asserta(location(window, kitchen)),
	asserta(location(knife, kitchen)),
	asserta(location(flowerbed, 'front yard')),
	asserta(location(paper, flowerbed)),
	asserta(location(toilet, bathroom)),
	asserta(location(sink, bathroom)),
	asserta(location(bench, reception)),
	asserta(location(plant, reception)),
	asserta(location(desk, office)),
	asserta(location('photocopy machine', office)),
	asserta(location('water dispenser', office)),
	asserta(location('coffee maker', office)),
	asserta(location(endtable, office)),
	asserta(location('sticky notes', desk)),
	asserta(location(document, desk)),
	asserta(location('co-workers', office)),
	asserta(location(counter, reception)),
	asserta(location(stove,kitchen)),
	asserta(location(sofa,'living room')),
	asserta(cold(leftover)),
	asserta(warm(soup)),
	asserta(here(bedroom)),
	asserta(atime(7,59)),
	asserta(money(0)),
	asserta(step(1)),
	asserta(stept(1)),
	asserta(attribute(worn)),
	asserta(attribute(hungry)),
	asserta(attribute(uninjured)),
	asserta(attribute(bladder)),
	asserta(attribute(healthy)),
	asserta(have(clothes)),
	asserta(have(watch)).
	
delete_dynamic_facts :-
	retractall(location(_,_)),
	retractall(atime(_,_)),
	retractall(warm(_)),
	retractall(cold(_)),
	retractall(here(_)),
	retractall(money(_)),
	retractall(step(_)),
	retractall(attribute(_)),
	retractall(have(_)).
/* Is it food? */
is_food([soup, leftover]).
is_it_food(X) :- is_food(L), memberchk(X,L).


/* How does it taste & feel? */
tastes_bad(soup).
tastes_good(leftover).


/* Eat */

:- op(35,fx,eat).

eat(X) :- \+(have(X)), write('You don''t have the '), write(X), nl, plusoneminute(_,_), nl, !.

eat(X) :- have(X), attribute(full), write('You''re full'), nl, plusoneminute(_,_), nl, !.

eat(X) :- have(X), is_it_food(X), warm(X), tastes_bad(X), attribute(hungry), retract(have(X)), write('You have eaten the '), write(X), nl, plusPminute(_,_,10), nl, write('The '), write(X), write(' tastes bad'), retract(attribute(hungry)), asserta(attribute(full)), nl, !.

eat(X) :- have(X), is_it_food(X), warm(X), tastes_good(X), attribute(hungry), retract(have(X)), write('You have eaten the '), write(X), nl, plusPminute(_,_,10), nl, write('The '), write(X), write(' tastes good'), retract(attribute(hungry)), asserta(attribute(full)), nl, !.

eat(X) :- have(X), \+(is_it_food(X)), write('The '), write(X), write(' is not edible'), nl, plusoneminute(_,_), nl, !.

eat(X) :- have(X), is_it_food(X), \+(warm(X)), write('The '), write(X), write(' is cold.'), nl, plusoneminute(_,_), nl, !.


/* Connections Between Locations */
connected(X, Y) :- north(X, Y), fail.
connected(X, Y) :- north(Y, X).
connected(X, Y) :- west(X, Y), fail.
connected(X, Y) :- west(Y, X).
connected(X, Y) :- south(X, Y), fail.
connected(X, Y) :- south(Y, X).
connected(X, Y) :- east(X, Y), fail.
connected(X, Y) :- east(Y, X).
connected(X, Y) :- down(X, Y), fail.
connected(X, Y) :- down(Y, X).
connected(X, Y) :- up(X, Y), fail.
connected(X, Y) :- up(Y, X).
there_is_a_way(X,Y) :- way(X,Y), fail.
there_is_a_way(X,Y) :- way(Y,X).


/* Directions's Dropdown */
list_directions(Place) :- north_from_here(Place);
							south_from_here(Place); east_from_here(Place);
							west_from_here(Place);
							above_here(Place);
							below_here(Place), !.
list_directions(_).

north_from_here(Place) :- north(Dest,Place),
							write('The '), write(Dest),
							write(' is located north from the '), write(Place), nl, fail.
north_from_here.

south_from_here(Place) :- south(Dest,Place),
							write('The '), write(Dest),
							write(' is located south from the '), write(Place), nl, fail.
south_from_here.

east_from_here(Place) :- east(Dest,Place),
							write('The '), write(Dest), write(' is located east from the '), write(Place), nl, fail.
east_from_here.

west_from_here(Place) :- west(Dest,Place),
							write('The '), write(Dest), write(' is located west from the '), write(Place), nl, fail.
west_from_here.

above_here(Place) :- up(Dest,Place),
							write('The '), write(Dest), write(' is located above the '), write(Place), nl, fail.
above_here.

below_here(Place) :- down(Dest,Place),
							write('The '), write(Dest), write(' is located below the '), write(Place), nl, fail.
below_here.


/* Objects' Dropdown */

list_things(Place) :- location(Thing, Place), object(Thing, _, _),
							tab(2), write(Thing), nl, fail.
list_things(_).


/* NPC's Dropdown */

list_npcs(Place) :- location(Person, Place), npc(Person),
							tab(2), write(Person), nl, fail.
list_npcs(_).


/* Connections' Dropdown */

list_connections(Place) :- connected(Place, Place2),
							tab(2), write(Place2), nl, fail.
list_connections(_).


/* Look Around */

look :- here(Place), room(Place), Place == kitchen,
		write('KITCHEN'), nl,
		write('The kitchen is surprisingly clean considering the rest'), nl,
		write('of the house. The sink is clogged up and collecting water'), nl,
		write('and the noise of trickling water can be heard from the sink.'), nl,
		write('There is a microwave on one of the counters, and a fridge'), nl,
		write('sits on the end of them. There is a small window right above'), nl,
		write('right above the stove on one side of the room. The kitchen'), nl,
		write('is dimly lit with a light bulb, as small as the one in the'), nl,
		write('bathroom.'), nl, nl,
		list_directions(Place), nl,
		write('Objects in the '), write(Place), write(':'), nl,
		list_things(Place), nl,
		write('Subjects in the room:'), nl,
		list_npcs(Place), nl,
		write('Places to drive to:'), nl,
		list_way(Place), nl,
		write('Places to walk to:'), nl,
		list_connections(Place), !.

look :- here(Place), room(Place), Place == 'living room', nl,
		write('LIVING ROOM'), nl,
		write('You are in the living room. This is the main room of the '), nl,
		write('house. The mixed design of contemporary and modern'), nl,
		write('architecture had always fascinated you. In the morning,'), nl,
		write('light would enter through the windows, and color the room'), nl,
		write('with a warm hue. A comfy-looking sofa sits right in the'), nl,
		write('center, near the coffee table with an ID card on it.'), nl,
		write('A replica of the Mona Lisa hangs on one side of the wall.'), nl, nl,
		list_directions(Place), nl,
		write('Objects in the '), write(Place), write(':'), nl,
		list_things(Place), nl,
		write('Subjects in the room:'), nl,
		list_npcs(Place), nl,
		write('Places to drive to:'), nl,
		list_way(Place), nl,
		write('Places to walk to:'), nl,
		list_connections(Place), !.

look :- here(Place), room(Place), Place == bathroom,
		write('BATHROOM'), nl,
		write('You stepped into the bathroom. You can hear the water slowly'), nl,
		write('and rhythmically dripping from the faucet. The room is'), nl,
		write('is illuminated with a small lamp hanging off the ceiling.'), nl,
		write('It''s not much, but a bathroom is still a bathroom.'), nl,
		write('There is a sink right next to the door, a toilet on one'), nl,
		write('corner, and a shower on the other.'), nl,
		list_directions(Place), nl,
		write('Objects in the '), write(Place), write(':'), nl,
		list_things(Place), nl,
		write('Subjects in the room:'), nl,
		list_npcs(Place), nl,
		write('Places to drive to:'), nl,
		list_way(Place), nl,
		write('Places to walk to:'), nl,
		list_connections(Place), !.

look :- here(Place), room(Place), Place == bedroom,
		write('BEDROOM'), nl,
		write('This is the bedroom. Nobody would appreciate the absence of'), nl,
		write('order in this room. A dresser is placed right across the'), nl,
		write('bed, and a desk lies on one of the corner. The wooden floor'), nl,
		write('goes well with the light color of the walls.'), nl,
		list_directions(Place), nl,
		write('Objects in the '), write(Place), write(':'), nl,
		list_things(Place), nl,
		write('Subjects in the room:'), nl,
		list_npcs(Place), nl,
		write('Places to drive to:'), nl,
		list_way(Place), nl,
		write('Places to walk to:'), nl,
		list_connections(Place), !.

look :- here(Place), room(Place), Place == 'front yard',
		write('FRONT YARD'), nl,
		write('You went outside and saw the grassy front yard. A lovely'), nl,
		write('flowerbed grew along the front of the house. A car is parked'), nl,
		write('right outside the house. There is a dog kennel on the front'), nl,
		write('yard. The dog barks at you right as you stepped outside. The'), nl,
		write('neighborhood is nice and calm. There is barely anyone around'), nl,
		write('here, while only a few cars pass by occasionally.'), nl,
		list_directions(Place), nl,
		write('Objects in the '), write(Place), write(':'), nl,
		list_things(Place), nl,
		write('Subjects in the room:'), nl,
		list_npcs(Place), nl,
		write('Places to drive to:'), nl,
		list_way(Place), nl,
		write('Places to walk to:'), nl,
		list_connections(Place), !.

look :- here(Place), room(Place), Place == intersection,
		write('INTERSECTION'), nl,
		write('You pulled off and drove away. After a while,'), nl,
		write('you came at an intersection. To the North lies the highway '), nl,
		write('that leads out of town. The Smith & Co. building could be '), nl,
		write('seen on the East. To go back to the house, you could turn'), nl,
		write('around and went South.'), nl,
		list_directions(Place), nl,
		write('Objects in the '), write(Place), write(':'), nl,
		list_things(Place), nl,
		write('Subjects in the room:'), nl,
		list_npcs(Place), nl,
		write('Places to drive to:'), nl,
		list_way(Place), nl,
		write('Places to walk to:'), nl,
		list_connections(Place), !.

look :- here(Place), room(Place), Place == reception,
		write('RECEPTION'), nl,
		write('You are inside the Smith & Co. Building The word Smith & Co.'), nl,
		write('could be clearly seen written behind the reception. You'), nl,
		write('see the receptionist behind the counter. There''s an'), nl,
		write('elevator on the right side of the reception. A bench is'), nl,
		write('sitting on one of the corner. There''s nothing else in the'), nl,
		write('reception room though, aside from the potted plant to'), nl,
		write('somehow decorate the room.'), nl,
		list_directions(Place), nl,
		write('Objects in the '), write(Place), write(':'), nl,
		list_things(Place), nl,
		write('Subjects in the room:'), nl,
		list_npcs(Place), nl,
		write('Places to drive to:'), nl,
		list_way(Place), nl,
		write('Places to walk to:'), nl,
		list_connections(Place), !.

look :- here(Place), room(Place), Place == office,
		write('OFFICE'), nl, 
		write('You are in the office. You found a desk with the name'), nl,
		write('"George Weston" on it. There''s a photocopy machine right'), nl,
		write('next to the water dispenser. A coffee maker and laptop sits'), nl,
		write('on an endtable. You could clearly see and hear the sound of'), nl,
		write('your co-workers barely working and chatting by themselves.'), nl,
		list_directions(Place), nl,
		write('Objects in the '), write(Place), write(':'), nl,
		list_things(Place), nl,
		write('Subjects in the room:'), nl,
		list_npcs(Place), nl,
		write('Places to drive to:'), nl,
		list_way(Place), nl,
		write('Places to walk to:'), nl,
		list_connections(Place), !.

look :- here(Place), room(Place), Place == 'director''s cabin',
		write('DIRECTOR''S ROOM'), nl,
		write('You are in the director''s room. A big wooden desk lies'), nl,
		write('in the center of the room. A plaque with "Director" written'), nl,
		write('on it is sitting on the desk. There''s a big window looking'), nl,
		write('out right behind the desk.'), nl,
		list_directions(Place), nl,
		write('Objects in the Director''s Room'), write(':'), nl,
		list_things(Place), nl,
		write('Subjects in the room:'), nl,
		list_npcs(Place), nl,
		write('Places to drive to:'), nl,
		list_way(Place), nl,
		write('Places to walk to:'), nl,
		list_connections(Place), !.

/* Check */

:- op(35,fx,check).

check(Thing) :- Thing == bed, here(Place), location(Thing, Place),
				write('You slowly bent down and saw the corpse of the man who'), nl,
				write('lived in this house. His eyes were bloodied and looking'), nl,
				write('disturbingly straight at you.'), nl, nl,
				write('Under the bed, you can see:'), nl,
				list_things(Thing), assertz(here(Thing)), plusoneminute(_,_), nl, !.

check(Thing) :- Thing == drawer, here(Place), location(Thing, Place),
				write('You looked into the dresser. Inside the drawers...'), nl, nl,
				write('In the drawer, you can see'), nl,
				list_things(Thing), assertz(here(Thing)), plusoneminute(_,_), nl, !.
				
check(Thing) :- Thing == painting, here(Place), location(Thing, Place),
				write('You looked at the painting closely. It is a replica of Mona'), nl,
				write('Lisa, well made but still a replica. But the painting seemed'), nl,
				write('out of place; it doesn''t match the modern vibe given off'), nl,
				write('by this room in particular. Out of curiousity, you lifted the'), nl,
				write('painting and discovered a hidden safe behind the painting.'), nl, nl,
				write('Behind the painting, you can see:'), nl,
				list_things(Thing), assertz(here(Thing)), plusoneminute(_,_), nl, !.

check(Thing) :- Thing == fridge, here(Place), location(Thing, Place),
				write('You opened the fridge'), nl, nl,
				write('Inside the fridge, you can see:'), nl,
				list_things(Thing), assertz(here(Thing)), plusoneminute(_,_), nl, !.

check(Thing) :- Thing == corpse, here(Place), location(Thing, Place),
				write('You thoroughly searched every pocket in the corpse''s clothes.'), nl,
				write('After a careful search, you failed to find anything of worth'), nl,
				write('that you could take, except the gold ring on his ring finger.'), nl, nl,
				write('Inside the fridge, you can see:'), nl,
				list_things(Thing), assertz(here(Thing)), plusoneminute(_,_), nl, !.

check(Thing) :- Thing == flowerbed, here(Place), location(Thing, Place),
				write('As you look closer,'), nl, nl,
				write('You can see:'), nl,
				list_things(Thing), assertz(here(Thing)), plusoneminute(_,_), nl, !.

check(Thing) :- Thing == safe, here(Place), location(Thing, Place), have(paper),
				write('You tried the number sequence written on the piece of paper.'), nl,
				write('You heard a click from the safe and pulled the handle.'), nl, nl,
				write('You can see:'), nl,
				list_things(Thing), assertz(here(Thing)), plusoneminute(_,_), nl, !.

check(Thing) :- Thing == safe, here(Place), location(Thing, Place), \+(have(paper)),
				write('You don''t know the number combination.'), nl, plusoneminute(_,_), nl, !.

check(Thing) :- Thing == desk, here(Place), location(Thing, Place),
				write('It''s a medium-sized wooden desk. There''s a computer, stack'), nl,
				write('of papers, and lots of sticky notes scattered on the desk.'), nl,
				write('You can see:'), nl,
				list_things(Thing), assertz(here(Thing)), plusoneminute(_,_), nl, !.

check(_) :- write('There''s nothing interesting there...'), nl, plusoneminute(_,_), nl.


/* Inspect */

:- op(35,fx,inspect).

inspect(Thing) :- Thing == alarm, here(Place), location(Thing, Place),
				write('You walked closer to the source of the sound.'), nl,
				write('You saw a sticky note stuck on the alarm.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == knife, here(Place), location(Thing, Place),
				write('The knife has dried blood all over it, someone'), nl,
				write('should really clean it.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == drawer, here(Place), location(Thing, Place),
				write('It is a sturdy wooden dresser.'), nl,
				write('There are four drawers on the dressers.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == corpse, here(Place), location(Thing, Place),
				write('It''s the body of a poor man you killed last night.'), nl,
				write('You could clearly see that his throat had been cut'), nl,
				write('open and a pool of blood had formed around the body.'), nl,
				write('His shirt had been soaked in blood and there were'), nl,
				write('dry blood stains all over him. You looked closer and'), nl,
				write('found that he is wearing a gold ring.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == shower, here(Place), location(Thing, Place),
				write('It''s not what you can call clean. The floor is'), nl,
				write('slippery due to lack of cleaning. At least there''s'), nl,
				write('no water dripping off the shower head. You should'), nl,
				write('take off your clothes if you want to shower.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == painting, here(Place), location(Thing, Place),
				write('There is a hidden safe behind the painting.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == safe, here(Place), location(Thing, Place),
				write('The safe is planted in the wall.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == microwave, here(Place), location(Thing, Place),
				write('The microwave looked as if it had not been'), nl,
				write('cleaned for a very long time.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == fridge, here(Place), location(Thing, Place),
				write('It''s a standard one-door fridge.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == card, here(Place), location(Thing, Place),
				write('It''s an ID Card and reads "George Weston".'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == tv, here(Place), location(Thing, Place),
				write('The TV is not particularly large.'), nl,
				write('It is just right for the size of this room.'), nl,
				write('It''s not the fancy TV that costs a fortune,'), nl,
				write('but it''s not exactly cheap either.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- cold(Thing), here(Place), location(Thing, Place),
				write('The '), write(Thing), write(' is cold, and you hate cold meal'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- warm(Thing), here(Place), location(Thing, Place),
				write('The '), write(Thing), write(' is warm, and you love warm meal'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == toilet, here(Place), location(Thing, Place),
				write('Yeah, it''s a toilet alright.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == sink, here(Place), location(Thing, Place),
				write('The faucet is constantly letting out drops of water. You felt'), nl,
				write('the urge to fix it after some while.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == car, here(Place), location(Thing, Place),
				write('It''s a ''95 Ford Fiesta. The paint is a little worn out,'), nl,
				write('or maybe it''s because of dirt covering the car.'), nl,
				write('It''s not much, but it gets the work done.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.				

inspect(Thing) :- Thing == 'car key', here(Place), location(Thing, Place),
				write('You need this to drive the car'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == bed, here(Place), location(Thing, Place),
				write('It''s just an ordinary bed'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == flowerbed, here(Place), location(Thing, Place),
				write('It''s full of white orchids. You caught the sweet scent of'), nl,
				write('them when the wind blows.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == paper, here(Place), location(Thing, Place),
				write('It contains a sequence of numbers.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == computer, here(Place), location(Thing, Place),
				write('It''s a completely generic computer. The exact same PC'), nl,
				write('that most people would see at libraries or internet cafes.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == bench, here(Place), location(Thing, Place),
				write('A metal bench, just like the ones you'),nl,
				write('expected to find on hospitals and airports.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == plant, here(Place), location(Thing, Place),
				write('It''s a completely ordinary potted plant.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == desk, here(Place), location(Thing, Place),
				write('It''s a medium-sized wooden desk. There''s a computer'),nl,
				write('on the desk, and lots of sticky notes'),nl,
				write('scattered and stuck to documents.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == 'sticky notes', here(Place), location(Thing, Place),
				write('There''s a lot of crossed-out notes, and only one stood out. It reads '),nl,
				write('"Get the Director''s Signature". The note is stuck to a paper with '),nl,
				write('an empty signature box on the bottom.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == document, here(Place), location(Thing, Place),
				write('It''s an unsigned document.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == 'photocopy machine', here(Place), location(Thing, Place),
				write('It kept making that disturbing noise when someone uses it. It upsets you.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == 'water dispenser', here(Place), location(Thing, Place),
				write('It sits right in the center of a messy pile of used plastic cups.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == 'coffee maker', here(Place), location(Thing, Place),
				write('The coffee maker sat quietly on the endtable.'),nl,
				write('It''s strange that nobody seemed to use it.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == endtable, here(Place), location(Thing, Place),
				write('It is as if its sole purpose in life is to support'),nl,
				write('the coffee maker.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == counter, here(Place), location(Thing, Place),
				write('A wooden counter, it''s the most organic thing you could'),nl,
				write('see here, along with the plan.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == stove, here(Place), location(Thing, Place),
				write('The stove had numerous colorful stains all over the top side.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == window, here(Place), location(Thing, Place),
				write('The view outside is blurred with the oil and '),nl,
				write('stains stuck on the inner side of the window.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == sofa, here(Place), location(Thing, Place),
				write('It is not the usual leather sofa. The sofa is covered in velvet,'),nl,
				write('which means it doesn''t produce the squishy sound that you hate.'), nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- Thing == note, here(Place), location(Thing, Place),
				write('You looked at the sticky note on the alarm. It reads, '),nl,
				write('"Board of Directors Meeting @08:00"'),nl,
				assertz(here(Thing)), plusoneminute(_,_), nl, !.

inspect(Thing) :- write('It''s a completely ordinary '), write(Thing), write('. '), nl,
				write('You''re wasting your time by inspecting it.'), plusoneminute(_,_), nl, !.


/* Use something */

:- op(35,fx,use).

use(Thing) :- Thing == microwave, here(Place), location(Thing, Place), have(Food), cold(Food), retract(cold(Food)), asserta(warm(Food)), write('You have warmed the '), write(Food), nl, plusoneminute(_,_), nl, !.

use(Thing) :- Thing == shower, attribute(fresh), write('You have taken a shower'), nl, plusoneminute(_,_), nl, !.
use(Thing) :- Thing == shower, have(clothes), write('You''re still wearing clothes'), nl, plusoneminute(_,_), nl, !.
use(Thing) :- Thing == shower, here(Place), location(Thing, Place), attribute(worn), \+(have(clothes)), retract(attribute(worn)), asserta(attribute(fresh)), write('You took a cold and invigorating shower'), nl, plusPminute(_,_,10), nl, !.

use(Thing) :- Thing == toilet, attribute(relieved), write('You don''t need to use the toilet'), nl, plusoneminute(_,_), nl, !.
use(Thing) :- Thing == toilet, here(Place), location(Thing, Place), retract(attribute(bladder)), asserta(attribute(relieved)), write('Great idea. You quickly heeded the call of nature.'), nl, plusPminute(_,_,10), nl, !.

use(Thing) :- Thing == sink, here(Place), location(Thing, Place), write('The sink is broken.'), nl, plusoneminute(_,_), nl, !.

use(Thing) :- Thing == stove, here(Place), location(Thing, Place), \+(have(Food)), \+(cold(Food)), write('You don''t have anything to be heated.'), nl, plusoneminute(_,_), nl, !.

use(Thing) :- Thing == stove, here(Place), location(Thing, Place), have(Food), cold(Food),  write('You tried turning on the stove, but it won''t turn on.'), nl, plusoneminute(_,_), nl, !.

use(Thing) :- Thing == tv, here(Place), location(Thing, Place),
write('You spent some time on television. There was news about an escaped'), nl,
write('mental hospital patient. People were told to keep an eye out for him.'), nl,
plusPminute(_,_,20), nl, !.

use(Thing) :- Thing == computer, here(Place), location(Thing, Place),
write('When you turned on the computer, it prompted for a password. After'), nl,
write('trying everything that came to mind, you''re still unable to unlock it'), nl,
write('and decided to turn it back off.'), nl,
plusPminute(_,_,5), nl, !.

use(Thing) :- Thing == knife, have(Thing),
retract(attribute(healthy)), asserta(attribute(stabbed)),
plusPminute(_,_,5), nl, !, lose.

use(_) :- write('That''s not something you can use right now'), nl, plusoneminute(_,_), nl, !.


/* Go to direction */
can_go_west(X) :- here(Y), west(X, Y), !.
can_go_west(_) :- write('You cannot go there from here'), nl, !, fail;true.
go_west :- can_go_west(X), have(clothes), move(X), look.
go_west :- \+(have(clothes)), write('You really should wear your clothes before going.'), nl, !, fail;true.
w :- go_west, plusoneminute(_,_), nl, !.

can_go_east(X) :- here(Y), east(X, Y), !.
can_go_east(_) :- write('You cannot go there from here'), nl, !, fail;true.
go_east:- can_go_east(X), have(clothes), move(X), look.
go_east :- \+(have(clothes)), write('You really should wear your clothes before going.'), nl, !, fail;true.
e :- go_east, plusoneminute(_,_), nl, !.

can_go_north(X) :- here(Y), north(X, Y), !.
can_go_north(_) :- write('You cannot go there from here'), nl, !, fail;true.
go_north :- can_go_north(X), have(clothes), move(X), look.
go_north :- \+(have(clothes)), write('You really should wear your clothes before going.'), nl, !, fail;true.
n :- go_north, plusoneminute(_,_), nl, !.

can_go_south(X) :- here(Y), south(X, Y), !.
can_go_south(_) :- write('You cannot go there from here'), nl, !, fail;true.
go_south :- can_go_south(X), have(clothes), move(X), look.
go_south :- \+(have(clothes)), write('You really should wear your clothes before going.'), nl, !, fail;true.
s :- go_south, plusoneminute(_,_), nl, !.

can_go_up(X) :- here(Y), up(X, Y), !.
can_go_up(_) :- write('You cannot go up from here'), nl, !, fail;true.
go_up :- can_go_up(X), have(clothes), move(X), look.
go_up :- \+(have(clothes)), write('You really should wear your clothes before going.'), nl, !, fail;true.
u :- go_up, plusoneminute(_,_), nl, !.

can_go_down(X) :- here(Y), down(X, Y), !.
can_go_down(_) :- write('You cannot go down from here'), nl, !, fail;true.
go_down :- can_go_down(X), have(clothes), move(X), look.
go_down :- \+(have(clothes)), write('You really should wear your clothes before going.'), nl, !, fail;true.
d :- go_down, plusoneminute(_,_), nl, !.

move(X) :- retractall(here(_)), asserta(here(X)).


/* Inventory */

list_inventory(X) :- have(X), tab(2), write(X), nl, fail; true.
inventory :- write('Objects in your inventory:'), nl,
			list_inventory(_); true.

/* Stolen Goods' Value */
howmuch :- money(X), write('$'), write(X), nl, !.

/* Take something */

:- op(35,fx,take).

can_take(Thing) :- here(Place), location(Thing, Place), object(Thing,small,_), !.

can_take(Thing) :- here(Place), location(Thing, Place), object(Thing,large,_),
							write('The '), write(Thing), write(' is too large to take.'), nl, plusoneminute(_,_), nl,  !, fail.

can_take(Thing) :- here(Place), \+(location(Thing,Place)),
							write('There''s no '), write(Thing), write(' to take here.'), nl, plusoneminute(_,_), nl, !, fail.
can_take(_) :- !.


take_object(X) :- retract(location(X, _)), assertz(have(X)),
						write('You took the '), write(X), nl, plusoneminute(_,_), nl.


take(X) :- can_take(X), take_object(X), object(X,_,Value), retract(money(OldSum)), NewSum is OldSum+Value, asserta(money(NewSum)).
take(_).

/* Drop something */

:- op(35,fx,drop).

can_drop(clothes) :- \+(here(bathroom)), write('You can''t take of your clothes unless you''re in the bathroom.'), nl, plusoneminute(_,_), nl, !, fail;true.

can_drop(Thing) :- have(Thing), !.

can_drop(Thing) :- write('You cannot drop '), write(Thing), write('.'), nl, plusoneminute(_,_), nl, !, fail;true.

drop_object(X) :- retract(have(X)), here(Location), assertz(location(X, Location)),
						write('You have dropped the '), write(X), nl,
						plusoneminute(_,_), nl.

drop(X) :- can_drop(X), drop_object(X), object(X,_,Value), retract(money(OldSum)), NewSum is OldSum-Value, asserta(money(NewSum)), command_loop.						
drop(_).
						
is_contained_in(T1, T2) :- location(T1, T2).

is_contained_in(T1, T2) :- location(X, T2),
									is_contained_in(T1, X).


/* Ways' Dropdown */

list_way(Place) :- there_is_a_way(Place, Place2),
					tab(2), write(Place2), nl, fail.
list_way(_).



/* Drive car */

:- op(35,fx,drive).

drive(_) :- \+(have('car key')),
			write('You don''t have the car key.'), nl, plusoneminute(_,_), nl, !.

drive(Place) :- have('car key'), here(X), \+(there_is_a_way(X,Place)),
			write('You cannot drive there'), nl, plusoneminute(_,_), nl, !.

drive(Place) :- have('car key'), here(X), there_is_a_way(X, Place), move(Place),
			write('You drove to '), write(Place), nl, retract(location(car,_)), asserta(location(car,Place)),
			plusPminute(_,_,15), (look ; win), nl, !.




/* Instructions (Help) */

instructions :-
	write('This is the list of commands that you can give'), nl,
	tab(2), write('n.                           go to north'), nl,
	tab(2), write('e.                           go to east'), nl,
	tab(2), write('w.                           go to west'), nl,
	tab(2), write('s.                           go to south'), nl,
	tab(2), write('u.                           go up'), nl,
	tab(2), write('d.                           go down'), nl,
	tab(2), write('look.                        look around'), nl,
	tab(2), write('stat.                        to see player''s attribute'), nl,
	tab(2), write('inventory.                   check your belongings'), nl,
	tab(2), write('save name.                   to save your progress with name "name"'), nl,
	tab(2), write('load name.                   to load your progress with name "name"'), nl,
	tab(2), write('inspect something.           ex: inspect soup'), nl,
	tab(2), write('check something.             ex: check bed'), nl,
	tab(2), write('take something.              ex: take gold'), nl,
	tab(2), write('drop something.              ex: drop clothes'), nl,
	tab(2), write('drive somewhere.             ex: drive intersection'), nl,
	tab(2), write('strangle someone.            sstt, be careful!'), nl,
	tab(2), write('eat something.               ex: eat soup'), nl,
	tab(2), write('talk someone.                ex: talk dog'), nl,
	tab(2), write('credits.                     show the credits'), nl,
	tab(2), write('quit.                        exit from game'), nl,
	write('Use '' '' if you use more than 1 word, ex: take ''dog food''.'), nl,
	write('Press "enter" key to continue'), nl,
	get0(_),
	look.


/* Strangle (corpse) */

:- op(35,fx,strangle).

strangle(X) :- X == corpse, here(Place), location(X, Place), write('You put your hands around the '), write(X), write('''s neck and strangled him one more time. You just had to make sure, right?'), nl, plusoneminute(_,_), nl, !.

strangle(X) :- X == 'co-workers', here(Place), location(X, Place), losestrangle, !.

strangle(X) :- X \== corpse, write('You cannot strangle '), write(X), nl, plusoneminute(_,_), nl, !.

strangle(X) :- X == corpse, here(Place), \+(location(X, Place)), room(Place), write('You cannot strangle '), write(X), write(' from the '), write(Place), nl, plusoneminute(_,_), nl, !.


/* Director's Cabin (a.k.a. bad ending)*/

lose :- here('director''s cabin'), step(X), X == 1, write('"Hey, what are you doing in here?" the man asked you.'), nl, plusonestep(X,Y), retract(step(X)), assertz(step(Y)), write('> '), read(Z), do(Z), !, lose.
lose :- step(X), X == 2,
	write('"Wait, who are you? I never saw you before" the man walked closer to you.'), nl, plusonestep(X,Y), retract(step(X)), assertz(step(Y)), write('> '), read(Z), do(Z), !, lose.
lose :- step(X), X == 3,
	write('The man noticed the paper you''re carrying "That''s the one I was looking for." He grabbed it from your hands. "Weston''s supposed to give this to me this morning, who are you?"'), nl, plusonestep(X,Y), retract(step(X)), assertz(step(Y)), write('> '), read(Z), do(Z), !, lose.
lose :- step(X), X == 4,
	write('"I''m gonna call the security." The man reached for the phone.'), nl, plusonestep(X,Y), retract(step(X)), assertz(step(Y)), write('> '), read(Z), do(Z), !, lose.
lose :- step(X), X == 5,
	write('A short time later, the security came in and took you away.'), nl, plusonestep(X,Y), retract(step(X)), assertz(step(Y)), write('> '), read(Z), do(Z), !, lose.
lose :- step(X), X == 6, have(card),
  	write('You were brought to the police office. They found an ID card of George '),nl,
  	write('Weston with you and sent some officers to check the house. The police '),nl,
  	write('swept the house and found the body of George Weston under the bed. '),nl,
  	write('You were arrested as soon as they received the report.'),nl,
  	write('You would be facing a 20 year charge for first-degree murder.'),nl,abort.

lose :- step(X), X == 6, \+(have(card)),
  write('You were brought to the police office. They could not find any sort '),nl,
  write('of ID on you. You were jailed for about 4 hours before they finished '),nl,
  write('the background check on you. The police discovered that you have no '),nl,
  write('criminal record. They investigated where you got the keycard. You lied '),nl,
  write('your that you found it on the reception and went exploring. '),nl,
  write('They did not believe your words. They did a mental check and found '),nl,
  write('that you are suffering from schizophrenia. They let you off with a '),nl,
  write('warning and referred you to a mental hospital downtown.'),nl,abort.


lose :- atime(H,_), H >= 14, here(P), house(L), memberchk(P,L),
	write('As you minded your own business, you faintly heard the sound of '), nl,
	write('police siren coming from the city. The sound gradually grew louder until '), nl,
	write('you can see the police car coming to a stop in front of your house. '), nl,
	write('Armed officers came out of the vehicle and forcefully entered your house. '), nl,
	write('In your confusion, you made no attempt to escape and you were arrested. '), nl,
	write('The police swept the house and found the body of George Weston under the bed.'), nl,
	write('Apparently, the neighbors grew concerned when they saw an unknown person '), nl,
	write('doing suspicious activities in the house of George Weston. '), nl ,abort.
lose :- atime(H,_), H >= 14, here(P), workplace(L), memberchk(P,L),
	write('As you minded your own business, you faintly heard the sound of'), nl,
	write('police siren coming from the city. The sound gradually grew louder until '), nl,
	write('you can see the police car coming to a stop in front of the workplace. '), nl,
	write('Armed officers came out of the vehicle and forcefully entered your workplace. '), nl,
	write('In your confusion, you made no attempt to escape and you were arrested. '), nl,
	write('The police had received complaints of suspicious activities of an unknown'), nl,
	write('person, both at the house of George Weston and his workplace, and had subsquently'), nl,
	write('swept the house, where they found the body of George Weston under the bed.'),nl,
	write('You would be facing a 20 year charge for first-degree murder.'), nl, abort.
lose :- atime(H,_), H >= 14, here(intersection),
	write('As you drove, you faintly heard the sound of police sirens coming from behind you.'), nl,
	write('A police car came to behind your car and hailed you to stop. Seeing no reason not'), nl,
	write('to comply, you stopped your vehicle on the side of the road. Suddenly, armed officers'), nl,
	write('came out of the vehicle and told you to put your hands up. Apparently, the police'), nl,
	write('had received complaints of suspicious activities of an unknown person at the house'), nl,
	write('of George Weston and had subsquently swept the house, where they found the body of'), nl,
	write('George Weston under the bed. You would be facing a 20 year charge for first-degree'), nl,
	write('murder.'), nl, abort.

lose :- attribute(dead), here('front yard'),
		write('You walked closer to the dog, stretching out your bleeding hand to try and pet'), nl,
		write('the furry beast. The dog growled as you got closer, baring his teeth at you.'), nl,
		write('When it finally leapt forward, it was already too late. The sharp teeth found'), nl,
		write('their way to your face and that''s the last thing you see.'), nl, nl,		
		write('Your obsession to talk with the dog, for whatever reason, has finally led'), nl,
		write('to the untimely and grisly demise of your life.'), nl, abort.

lose :- attribute(stabbed),
		write('You slowly brought the knife closer to your neck. The sound on in your head'), nl,
		write('told you to drag it across the neckand it would all be over. Somehow, that'), nl,
		write('sounded like a good idea at the time. You slowly felt cold starting from your toes.'), nl,
		write('The cold started to find its way up your legs. By the time you felt cold on your'), nl, nl,		
		write('torso, you were already collapsed on the ground. You felt warm blood on the side of'), nl,
		write('face, fresh from the wound on your neck. The sound in your head slowly faded away.'), nl,
		write('You have succeeded in ending your own life.'), nl, abort.

losestrangle :- 
		write('You went and strangled one unlucky co-worker in the office. The rest of the group'), nl, 
		write('just froze and watched the two of you. When the man finally turned blue, one of'), nl,
		write('them grabbed a monitor and, before you know it, smashed the monitor against'), nl, 
		write('your head. When you came to, you had been arrested by the police. You have been'), nl, 
		write('charged with attempted murder of the man you strangled. You are now facing 10 years'), nl,
		write('of prison.'), nl, nl, 
		write('After a week in prison, you have been charged again with the murder of George Weston.'), nl, 
		write('The police raided his house and found his body under the bed. They found your fingerprints'), nl,
		write('on his body and around the house. You would be jailed for a total of 30 years'), nl,
		write('for both charges.'), nl, abort.


/* NPC */

:- op(35,fx,talk).

talk(Person) :- Person == director, npc(director).

talk(Person) :- Person == receptionist, npc(Person), \+(have(card)),
				here(Place), location(Person, Place),
				write('You walked to the reception and stopped in front of'), nl,
				write('the counter. You don''t know what to say to the'), nl,
				write('receptionist. The young woman looked at you,'), nl,
				write('confused. "I don''t know you," she said.'), nl, plusoneminute(_,_), nl, !.
talk(Person) :- Person == receptionist, npc(Person), have(card),
				here(Place), location(Person, Place),
				write('"Err, Mr Weston, the director''s been looking for'), nl,
				write('you. You should go to his room.'), nl, plusoneminute(_,_), nl, !.

talk(Person) :- Person == 'co-workers', npc(Person), stept(X), X == 1, plusonestep(X,Y), retract(stept(X)), assertz(stept(Y)),
				here(Place), location(Person, Place),
				write('"You guys should have seen Dave here last night. He''s so drunk'), nl,
				write('that he went headfirst to the bathtub, thinking it was the bed."'), nl,
				write('Laughter came from every individual in that group. When they'), nl,
				write('finally noticed you, they had stopped laughing and focused on you.'), nl, plusoneminute(_,_), nl, !.
talk(Person) :- Person == 'co-workers', npc(Person), stept(X), X == 2, plusonestep(X,Y), retract(stept(X)), assertz(stept(Y)),
				here(Place), location(Person, Place),
				write('"Hey man, are you new? I had never seen you around here before."'), nl,
				write('You could see the other co-workers whispering to each other.'), nl, plusoneminute(_,_), nl, !.
talk(Person) :- Person == 'co-workers', npc(Person), stept(X), X == 3, plusonestep(X,Y), retract(stept(X)), assertz(stept(Y)),
				here(Place), location(Person, Place),
				write('One of the other co-worker finally spoke up, "Well, welcome to'), nl,
				write('the office anyway. Some of us are new too - the director'), nl,
				write('really likes to fire people. Don''t let him catch you off'), nl,
				write('guard." The sound of laughter roared yet again in the group.'), nl, plusoneminute(_,_), nl, !.
talk(Person) :- Person == 'co-workers', npc(Person), stept(X), X == 4,
				here(Place), location(Person, Place),
				write('They continued to talk about the party last night. You started'), nl,
				write('to regret about not going to the party.'), nl, plusoneminute(_,_), nl, !.

talk(Animal) :- Animal == dog, npc(Animal), attribute(bitten),
				here(Place), location(Animal, Place),
				retract(attribute(bitten)), asserta(attribute(dead)),
				plusoneminute(_,_), !.
talk(Animal) :- Animal == dog, npc(Animal), \+(have('dog food')), attribute(uninjured),
				here(Place), location(Animal, Place),
				write('The dog kept barking at you. You tried to calm it'), nl,
				write('down. As you walked closer, the dog suddenly leapt'), nl,
				write('at you. Startled, you quickly jumped away, but'), nl,
				write('not quick enough unfortunately. Your hand got bitten'), nl,
				write('and let out a trickle of blood.'), nl, plusoneminute(_,_),
				retract(attribute(uninjured)), asserta(attribute(bitten)), nl, !.
talk(Animal) :- Animal == dog, npc(Animal), have('dog food'),
				here(Place), location(Animal, Place),
				write('The dog ate its food calmly.'), nl,
				retract(have('dog food')), plusoneminute(_,_), nl, !.


/* Save Load */
listpret([],_).
listpret([X|T],Eks) :-
  write(Eks,X), nl(Eks), listpret(T,Eks).

save_inventory(X,Eks) :- findall(X,have(X),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_cold(X,Eks) :- findall(X,cold(X),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_warm(X,Eks) :- findall(X,warm(X),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_attribute(X,Eks) :- findall(X,attribute(X),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_bedroom(X,Eks) :- findall(X,location(X,bedroom),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_frontyard(X,Eks) :- findall(X,location(X,'front yard'),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_livingroom(X,Eks) :- findall(X,location(X,'living room'),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_kitchen(X,Eks) :- findall(X,location(X,kitchen),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_bathroom(X,Eks) :- findall(X,location(X,bathroom),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_directorscab(X,Eks) :- findall(X,location(X,'director''s cabin'),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_office(X,Eks) :- findall(X,location(X,office),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_reception(X,Eks) :- findall(X,location(X,reception),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_car(X,Eks) :- findall(X,location(X,car),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_drawer(X,Eks) :- findall(X,location(X,drawer),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_painting(X,Eks) :- findall(X,location(X,painting),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_safe(X,Eks) :- findall(X,location(X,safe),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_fridge(X,Eks) :- findall(X,location(X,fridge),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_corpse(X,Eks) :- findall(X,location(X,corpse),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.
save_corpse(X,Eks) :- findall(X,location(X,corpse),Y), length(Y,B), write(Eks,B), nl(Eks),
                          (B > 0) -> listpret(Y,Eks); true.

:- op(35,fx,save).

save(FILE) :-
  open(FILE,write,ID),
    (
    (
      atime(HR,MN), write(ID,HR), nl(ID), write(ID,MN), nl(ID),
      here(Position), write(ID,Position), nl(ID),
      save_inventory(_,ID),
			save_cold(_,ID),
			save_warm(_,ID),
			save_attribute(_,ID),
			save_bedroom(_,ID),
			save_frontyard(FThing,ID),
			save_livingroom(_,ID),
			save_kitchen(_,ID),
			save_bathroom(_,ID),
			save_directorscab(DThing,ID),
			save_office(_,ID),
			save_reception(_,ID),
			save_car(_,ID),
			save_drawer(DThing,ID),
			save_painting(_,ID),
			save_safe(_,ID),
			save_fridge(FThing,ID),
			save_corpse(_,ID),
			money(Money), write(ID,Money), nl(ID),
			write('Game has been saved'),nl
		)
    , close(ID)
    ).
load_inventory(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(have(Thing)), fail ; true.
load_cold(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(cold(Thing)), fail ; true.
load_warm(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(warm(Thing)), fail ; true.
load_attribute(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(attribute(Thing)), fail ; true.
load_bedroom(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(location(Thing,bedroom)), fail ; true.
load_frontyard(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(location(Thing,'front yard')), fail ; true.
load_livingroom(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(location(Thing,'living room')), fail ; true.
load_kitchen(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(location(Thing,kitchen)), fail ; true.
load_bathroom(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(location(Thing,bathroom)), fail ; true.
load_directorscab(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(location(Thing,'director''s cabin')), fail ; true.
load_office(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(location(Thing,office)), fail ; true.
load_reception(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(location(Thing,reception)), fail ; true.
load_car(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(location(Thing,car)), fail ; true.
load_drawer(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(location(Thing,drawer)), fail ; true.
load_painting(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(location(Thing,painting)), fail ; true.
load_safe(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(location(Thing,safe)), fail ; true.
load_fridge(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(location(Thing,fridge)), fail ; true.
load_corpse(Eks,N) :- for(_,1,N), readWord(Eks,Thing), asserta(location(Thing,corpse)), fail ; true.

:- op(35,fx,load).

loads(FILE) :-
		catch(open(FILE,read,ID),error(existence_error(_,_),_), (write('File not found'), nl, fail, !)),
			(
			(
				readWord(ID,HR), readWord(ID,MN), retractall(atime(_,_)), number_atom(HRC,HR), number_atom(MNC,MN), asserta(atime(HRC,MNC)),
				readWord(ID,Position), retractall(here(_)), asserta(here(Position)),
				readWord(ID,NInven), retractall(have(_)), number_atom(NInvenC,NInven), load_inventory(ID,NInvenC),
				readWord(ID,NFCold), retractall(cold(_)), number_atom(NFColdC,NFCold), load_cold(ID,NFColdC),
				readWord(ID,NFWarm), retractall(warm(_)), number_atom(NFWarmC,NFWarm), load_warm(ID,NFWarmC),
				readWord(ID,NAttribute), retractall(attribute(_)), number_atom(NAttributeC,NAttribute), load_attribute(ID,NAttributeC),
				readWord(ID,NBed), retractall(location(_,bedroom)), number_atom(NBedC,NBed), load_bedroom(ID,NBedC),
				readWord(ID,NFront), retractall(location(_,'front yard')), number_atom(NFrontC,NFront), load_frontyard(ID,NFrontC),
				readWord(ID,NLiving), retractall(location(_,'living room')), number_atom(NLivingC,NLiving), load_livingroom(ID,NLivingC),
				readWord(ID,NKitchen), retractall(location(_,kitchen)), number_atom(NKitchenC,NKitchen), load_kitchen(ID,NKitchenC),
				readWord(ID,NBath), retractall(location(_,bathroom)), number_atom(NBathC,NBath), load_bathroom(ID,NBathC),
				readWord(ID,NDirector), retractall(location(_,'director''s cabin')), number_atom(NDirectorC,NDirector), load_directorscab(ID,NDirectorC),
				readWord(ID,NOffice), retractall(location(_,office)), number_atom(NOfficeC,NOffice), load_office(ID,NOfficeC),
				readWord(ID,NReception), retractall(location(_,reception)), number_atom(NReceptionC,NReception), load_reception(ID,NReceptionC),
				readWord(ID,NCar), retractall(location(_,car)),  number_atom(NCarC,NCar), load_car(ID,NCarC),
				readWord(ID,NDrawer), retractall(location(_,drawer)), number_atom(NDrawerC,NDrawer), load_drawer(ID,NDrawerC),
				readWord(ID,NPaint), retractall(location(_,painting)), number_atom(NPaintC,NPaint), load_painting(ID,NPaintC),
				readWord(ID,NSafe), retractall(location(_,safe)), number_atom(NSafeC,NSafe), load_safe(ID,NSafeC),
				readWord(ID,NFridge), retractall(location(_,fridge)), number_atom(NFridgeC,NFridge), load_fridge(ID,NFridgeC),
				readWord(ID,NCorpse), retractall(location(_,corpse)), number_atom(NCorpseC,NCorpse), load_corpse(ID,NCorpseC),
				readWord(ID,Money), retractall(money(_)), number_atom(MoneyC,Money), asserta(money(MoneyC)),
				write('Game has been loaded'),nl,nl,look
			)
			, close(ID)
			).

readWord(InStream,W):-
  get_code(InStream,Char),
	checkCharAndReadRest(Char,Chars,InStream),
	atom_codes(W,Chars).

checkCharAndReadRest(10,[],_):- !.
checkCharAndReadRest(-1,[],_):- !.
checkCharAndReadRest(end_of_file,[],_):- !.
checkCharAndReadRest(Char,[Char|Chars],InStream):- get_code(InStream,NextChar),
  checkCharAndReadRest(NextChar,Chars,InStream).