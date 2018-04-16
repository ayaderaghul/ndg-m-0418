# ndg-m-0418
markov chain in automaton structure
todo: doing automaton structure (from pd to ndg) -> done
action-plan: list of 3 lists of 3

log: random-decimal: better version

if n = 0, return 0, else ...

old version: if prob = 0, return 0, else ...

-> problematic


log: main: better version

old version: enter manually the input population file name

new version: automatically, wrt sim-id

todo: name the pic w configurations, it gets confusing

- apply these changes to backward versions, or combine them..

p: calculate the delta sequence first, then use it for the rest
no need to expt them every time

round1: try this instead of round2


log: so i discover serious issue.
the what-next? function. it is supposedly that what-next? has to take into account of what i do then what opponent does. which the matrix is inverse for player 2.
i guess PD does not have this problem bc i did not represent it as matrix. i hope.
i think all data so far is problematic.

so what-next? for person 1 is (what-next? action1 action2 plan1)
but what-next? for person 2 is (what-next? action2 action1 plan2)

as consequence, only use data3 onward.
