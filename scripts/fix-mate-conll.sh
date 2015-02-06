#!/bin/sh

awk 'BEGIN{OFS = "\t"}{print $1,$2,$4,$6,$6,$8,$10,$12}' $1
