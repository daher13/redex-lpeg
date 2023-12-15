#lang typed-peg/debug/infer-only

S <-- Q ;
Q <-- B;
B <-- '1' Q ;

start: S
