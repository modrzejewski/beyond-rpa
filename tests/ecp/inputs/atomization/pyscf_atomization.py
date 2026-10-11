"""
PySCF reference energies for the atomization test set with PP basis sets.

The basis sets are cc-pVTZ-PP on the elements with ECPs and cc-pVTZ on the
others; the built-in PySCF data equal bse/cc-pVTZ-PP and cc-repo/cc-pVTZ of
beyond-rpa. Closed shells are RHF, as in beyond-rpa, and open shells are UHF.
Every species starts from the default initial guess. An unstable solution of
an atom is followed until it is stable. A solution that is not converged or
not stable is an error.

The geometries and multiplicities of the hydrides are those of the DFT-D3
reference molecules of the Grimme group, with the coordinates converted from
bohr (1 bohr = 0.529177210903 Angstrom).

Usage: python pyscf_atomization.py
Progress is printed on the screen and the references are written to
pyscf_atomization.txt; then run inject_reference_preambles.py.
Requires pyscf (references were generated with pyscf 2.12.0).
"""
import time
from pathlib import Path

from pyscf import gto, scf

OUTPUT = Path(__file__).resolve().parent / "pyscf_atomization.txt"
BASIS = {"Hf": "cc-pvtz-pp", "Ta": "cc-pvtz-pp", "W": "cc-pvtz-pp", "O": "cc-pvtz", "H": "cc-pvtz"}
ECP = {"Hf": "cc-pvtz-pp", "Ta": "cc-pvtz-pp", "W": "cc-pvtz-pp"}
MAX_CYCLE = 256
STABILITY_ROUNDS = 10
SPECIES = [
    {
        "name": "hfo",
        "charge": 0,
        "multiplicity": 1,
        "geometry": """
            Hf  0.00000000  0.00000000  0.00000000
            O   0.00000000  0.00000000  1.70000000
        """,
    },
    {
        "name": "tao",
        "charge": 1,
        "multiplicity": 1,
        "geometry": """
            Ta  0.00000000  0.00000000  0.00000000
            O   0.00000000  0.00000000  1.70000000
        """,
    },
    {
        "name": "wo",
        "charge": 2,
        "multiplicity": 1,
        "geometry": """
            W   0.00000000  0.00000000  0.00000000
            O   0.00000000  0.00000000  1.70000000
        """,
    },
    {
        "name": "hfh2",
        "charge": 0,
        "multiplicity": 3,
        "geometry": """
            Hf  0.00000000  0.00000000 -0.74088040
            H   1.50747855  0.00000000  0.37044020
            H  -1.50747855  0.00000000  0.37044020
        """,
    },
    {
        "name": "hfh4",
        "charge": 0,
        "multiplicity": 1,
        "geometry": """
            Hf  0.00000000  0.00000000  0.00000000
            H   1.06685503 -1.06685503  1.06685503
            H  -1.06685503  1.06685503  1.06685503
            H   1.06685503  1.06685503 -1.06685503
            H  -1.06685503 -1.06685503 -1.06685503
        """,
    },
    #
    # W(H2)6, six side-on H2 ligands: the DFT-D3 structure wh2.coord
    # (0 unpaired electrons, coordination number 10.9)
    #
    {
        "name": "wh12",
        "charge": 0,
        "multiplicity": 1,
        "geometry": """
            W   0.00000000  0.00000000  0.00000000
            H   1.31759186 -1.04272642 -0.66598229
            H  -0.38940854  1.27265455 -1.21878224
            H   1.31759186  1.04272642  0.66598229
            H   1.29685544 -0.29908959 -1.21878224
            H  -0.38940854 -1.27265455  1.21878224
            H   0.24423164  1.66243123 -0.66598229
            H   1.29685544  0.29908959  1.21878224
            H  -0.90744690  0.97356496  1.21878224
            H  -0.90744690 -0.97356496 -1.21878224
            H  -1.56182350  0.61970481  0.66598229
            H   0.24423164 -1.66243123  0.66598229
            H  -1.56182350 -0.61970481 -0.66598229
        """,
    },
    {"name": "hf", "charge": 0, "multiplicity": 3, "geometry": "Hf 0.0 0.0 0.0"},
    {"name": "ta", "charge": 0, "multiplicity": 4, "geometry": "Ta 0.0 0.0 0.0"},
    {"name": "w", "charge": 0, "multiplicity": 5, "geometry": "W 0.0 0.0 0.0"},
    {"name": "o", "charge": 0, "multiplicity": 3, "geometry": "O 0.0 0.0 0.0"},
    {"name": "h", "charge": 0, "multiplicity": 2, "geometry": "H 0.0 0.0 0.0"},
]


def reference(system):
    """
    Return the HF solution and the number of stability rounds. An unstable
    solution of an atom is followed until it is stable. A solution that is
    not converged or not stable is an error.
    """
    mol = gto.M(
        atom=system["geometry"],
        charge=system["charge"],
        spin=system["multiplicity"] - 1,
        basis=BASIS,
        ecp=ECP,
        verbose=0,
    )
    mean_field = scf.RHF(mol) if mol.spin == 0 else scf.UHF(mol)
    mean_field.conv_tol = 1.0e-12
    mean_field.max_cycle = MAX_CYCLE
    mean_field.kernel()
    mo, _, stable, _ = mean_field.stability(return_status=True)
    rounds = 0
    while not stable and mol.natm == 1 and rounds < STABILITY_ROUNDS:
        rounds += 1
        mean_field.kernel(mean_field.make_rdm1(mo, mean_field.mo_occ))
        mo, _, stable, _ = mean_field.stability(return_status=True)
    if not mean_field.converged:
        raise RuntimeError(f"{system['name']}: SCF not converged")
    if not stable:
        raise RuntimeError(f"{system['name']}: the HF solution is not stable")
    return mean_field, rounds


def main():
    blocks = []
    for k, system in enumerate(SPECIES, start=1):
        print(f"[{k}/{len(SPECIES)}] {system['name']}... ", end="", flush=True)
        start = time.time()
        mean_field, rounds = reference(system)
        s2 = f", <S^2> = {mean_field.spin_square()[0]:.4f}" if mean_field.mol.spin != 0 else ""
        followed = f", stable after {rounds} stability rounds" if rounds > 0 else ""
        print(f"E = {mean_field.e_tot:.10f}{s2}{followed}  ({time.time() - start:.1f} s)", flush=True)
        blocks.append(
            f"Input: {system['name']}\n"
            f"Charge:                     {system['charge']}\n"
            f"Multiplicity:               {system['multiplicity']}\n"
            f"HF single point (a.u.):     {mean_field.e_tot:.10f}\n"
        )
    OUTPUT.write_text("\n".join(blocks))
    print(f"Wrote {OUTPUT}", flush=True)


if __name__ == "__main__":
    main()
