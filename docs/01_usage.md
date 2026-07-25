# Usage
## Running `beyond-rpa`
Execute the launcher script
```bash
./beyond-rpa/bin/run -nt 16 example.inp 
```
where
* `-nt 16` specifies the number of concurrent threads used by the program. To get the best
efficiency, the number of threads should be equal to the number of physical cores available
for your calculations. For example, if you reserved a single node with shared memory and
2 CPUs, each having 18 physical cores, then the optimal setting is `-nt 36`.
* `example.inp` is the input text file which contains the definition of the physical system
and the requested level of theory. Example inputs are given below.

## Examples
### 1. Noncovalent interaction energy of a water dimer
```
jobtype uks rpa
basis aug-cc-pVDZ
scf
 xcfunc HF
end

rpa
 accuracy default
 TheoryLevel RPA+ph
end

xyz
3 3
O      1.531750     0.005922    -0.120880
H      0.575968    -0.005249     0.024966
H      1.906249    -0.037561     0.763218
O     -1.396226    -0.004990     0.106766
H     -1.789372    -0.742283    -0.371009
H     -1.777037     0.777638    -0.304264
end
```

### 2. Nonadditive 3-body interaction energy of a formaldehyde trimer
```
jobtype uks rpa
basis aug-cc-pVDZ
scf
 xcfunc HF
end

rpa
 accuracy default
 TheoryLevel RPA+ph
end

xyz
4 4 4
O   2.074088   0.498855   1.635515
C   2.169454   0.154280   2.793163
H   2.970226  -0.523067   3.124239
H   1.470340   0.502963   3.564302
O   2.074088   0.498855   6.109515
C   2.169454   0.154280   7.267163
H   2.970226  -0.523067   7.598239
H   1.470340   0.502963   8.038302
O   2.074088   0.498855  10.583515
C   2.169454   0.154280  11.741163
H   2.970226  -0.523067  12.072239
H   1.470340   0.502963  12.512302
end
```
