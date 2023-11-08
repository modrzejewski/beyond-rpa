#!/usr/bin/python3
# Convert CSV spreadsheets to text files with formated ECP parameters
# Coded by Marcin Modrzejewski, January 2017
#
import argparse
import sys

parser = argparse.ArgumentParser()
parser.add_argument("input_file", help="path to the parameter text file", type=str)
args = parser.parse_args()
INPUT_FILE = args.input_file
                                             
f = open(INPUT_FILE, "r")
lines = f.readlines()
f.close()

data_start = 4
data_stop = len(lines) - 1

t1, Element, t2, t3, t4 = lines[0].split("&")
t1, Citation, t2, t3, t4 = lines[1].split("&")
t1, NCoreElectrons, t2, t3, t4 = lines[2].split("&")

LBlocksOrdering = ["S", "P", "D", "F", "G"]
AngularMomenta = {"S":0, "P":1, "D":2, "F":3, "G":4}
j_vals = {"S":["1/2", "1/2"], "P":["1/2", "3/2"], "D":["3/2", "5/2"], "F":["5/2","7/2"], "G":["7/2","9/2"]}
ECP_Coeffs = {}
LMax = 0

for k in range(data_start, data_stop+1):
    w = lines[k].split("&")
    if len(w) == 5:
        L, J, s1, s2, s3 = w
        L = L.upper()
        C = float(s1)
        Alpha = float(s2)
        N = int(s3)
        J_Hi = j_vals[L][1]
        J_Lo = j_vals[L][0]
        if L == "S":
            Cav = C
            Cso = 0.0
        elif L in AngularMomenta:
            lnum = AngularMomenta[L]
            if J == J_Hi:
                Cav = (lnum + 1.0) / (2.0 * lnum + 1.0) * C
                Cso = 2.0 / (2.0 * lnum + 1.0) * C
            elif J == J_Lo:
                Cav = lnum / (2.0 * lnum + 1.0) * C
                Cso = -2.0 / (2.0 * lnum + 1.0) * C
            else:
                print("Invalid j value")
                sys.exit(1)
        else:
            print("Unknown angular momentum")
            sys.exit(1)

        t = [Cav, Cso, N, Alpha]
        if L in ECP_Coeffs:
            ECP_Coeffs[L].append(t)
        else:
            ECP_Coeffs[L] = [t]
            LMax = max(LMax, AngularMomenta[L])
            
    elif len(w) > 0:
        print("Invalid number of columns")
        sys.exit(1)
        
print("{E}-Spin-Orbit-ECP {N} {L}".format(E=Element, N=NCoreElectrons, L=LMax+1))
print("!@Citation {C}".format(E=Element, C=Citation))
for L in LBlocksOrdering:
    if L in ECP_Coeffs:
        NGauss = len(ECP_Coeffs[L])
        print("{N} ---{L} block---".format(N=len(ECP_Coeffs[L]), L=L))
        #
        # Sort the exponents (Alpha) in descending order
        #
        for Cav, Cso, N, Alpha in sorted(ECP_Coeffs[L], key=lambda x: x[3], reverse=True):
            if L == "S":
                print("{C1:15.9f} {C2:15s} {C3:5d} {C4:15.9f}".format(C1=Cav, C2="", C3=N, C4=Alpha))
            else:
                print("{C1:15.9f} {C2:15.9f} {C3:5d} {C4:15.9f}".format(C1=Cav, C2=Cso, C3=N, C4=Alpha))








