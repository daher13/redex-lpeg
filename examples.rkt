#lang typed-peg/debug/infer-only

S <-- Q ;
Q <-- 'a' Q ;

start: S
