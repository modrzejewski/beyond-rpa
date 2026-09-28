# Examples

### 1. Noncovalent interaction energy of a water dimer
```text
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
```text
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

### 3. Dimer interaction energy with two different pseudopotentials

Calculation of the interaction energy of a heavy-atom dimer with distinct pseudopotentials on each center.

Key setup:
* **Quantum subsystem:** Hg...Xe dimer (`1 1`).
* **Basis set:** Uniform `def2-TZVP` assigned globally.
* **Pseudopotentials:** `ecp_assignment` specifies `cc-pVDZ-PP` for Hg and `def2-tzvp` for Xe.

```text
basis def2-TZVP

ecp_assignment
  Hg cc-pVDZ-PP
  Xe def2-tzvp
end

scf
  xcfunc HF
end

rpa
  TheoryLevel RPA+ph
  accuracy default
end

xyz
1 1
Hg 0.0 0.0 0.0
Xe 0.0 0.0 4.10
end
```

### 4. Interaction of an embedded dimer (`associate_with`)

Calculation of the noncovalent interaction energy of a water dimer where the embedding part belongs exclusively to monomer A.

Key setup:
* **Quantum subsystem:** Two water molecules defined in the `xyz` block (`3 3`).
* **Environment:** 16 point charges and 4 boundary Xe pseudopotentials.
* **Association:** `associate_with(A)` restricts the embedding potential to the dimer ($AB$) and monomer A ($A[\text{ghost } B]$), keeping the isolated monomer B ($B[\text{ghost } A]$) unperturbed.

```text
basis def2-SVP

scf
  xcfunc HF
end

rpa
  TheoryLevel RPA+ph
  accuracy default
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

embedding
point_charges(16) ecp_centers(4) associate_with(A)
Q(0.1) 4.12 -3.21 2.55
Q(-0.1) -4.55 2.11 -3.10
Q(0.1) 3.05 4.44 -2.15
Q(-0.1) -2.90 -4.80 1.50
Q(0.1) ECP(Xe def2-SVP) 5.10 0.25 0.85
Q(-0.1) ECP(Xe def2-SVP) -5.20 -0.15 -1.25
Q(0.1) 0.85 5.50 3.20
Q(-0.1) -0.75 -5.60 -2.80
Q(0.1) 2.20 -2.30 4.90
Q(-0.1) -1.80 3.40 -4.50
Q(0.1) ECP(Xe def2-SVP) 4.80 2.90 1.10
Q(-0.1) ECP(Xe def2-SVP) -3.70 -2.50 -1.70
Q(0.1) 1.50 -4.10 5.50
Q(-0.1) -1.10 5.20 -5.10
Q(0.1) 0.00 0.00 6.00
Q(-0.1) 0.00 0.00 -6.00
end
```


