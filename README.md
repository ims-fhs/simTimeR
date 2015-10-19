# simTimeR
Package to handle DateTimes as integers (called simDateTimes) for the smooth handling insiede simulations

simTimeR provides functions to coerce DateTimes to integer numbers (called simDateTime). The integer number represents seconds starting from 01.01.2014, 00:00:00. Also provides functions to access only the day of a simDateTime (as integer between 0 and 364) or only the time of a simDateTime (as integer between 0 and 24x60x60-1) or only the weekday as String (Mo, Di, Mi, Do, Fr, Sa, So). Also provides functions to cerce a simDateTime back to a normal Time Format.
