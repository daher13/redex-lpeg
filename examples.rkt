#lang typed-peg/debug/infer-only

S <-- Q ;
Q <-- '1';
B <-- Q ;

start: S
