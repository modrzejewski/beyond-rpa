"""
PySCF reference energies for the atomization test set with PP basis sets.

The basis sets are cc-pVTZ-PP on the elements with ECPs and cc-pVTZ on the
others; the built-in PySCF data equal bse/cc-pVTZ-PP and cc-repo/cc-pVTZ of
beyond-rpa. Closed shells are RHF, as in beyond-rpa, and open shells are UHF.
Every species starts from several initial guesses, and each solution is
followed through internal stability analysis until it is stable. The lowest
stable energy is the reference.

The geometries and multiplicities of the hydrides are those of the DFT-D3
reference molecules of the Grimme group, with the coordinates converted from
bohr (1 bohr = 0.529177210903 Angstrom).

Usage: python pyscf_atomization.py
Progress is printed on the screen and the references are written to
pyscf_atomization.txt; then run inject_reference_preambles.py.
Requires pyscf (references were generated with pyscf 2.12.0).
"""
from __future__ import annotations

import time
from pathlib import Path

from pyscf import gto, scf

OUTPUT = Path(__file__).resolve().parent / "pyscf_atomization.txt"
BASIS = {"Hf": "cc-pvtz-pp", "Ta": "cc-pvtz-pp", "W": "cc-pvtz-pp", "O": "cc-pvtz", "H": "cc-pvtz"}
ECP = {"Hf": "cc-pvtz-pp", "Ta": "cc-pvtz-pp", "W": "cc-pvtz-pp"}
INITIAL_GUESSES = ("minao", "atom", "huckel", "1e", "vsap")
MAX_CYCLE = 256
STABILITY_ROUNDS = 10
SPECIES = [
    {
        "name": "hfo",
        "multiplicity": 1,
        "geometry": """
            Hf  0.00000000  0.00000000  0.00000000
            O   0.00000000  0.00000000  1.70000000
        """,
    },
    {
        "name": "tao",
        "multiplicity": 2,
        "geometry": """
            Ta  0.00000000  0.00000000  0.00000000
            O   0.00000000  0.00000000  1.70000000
        """,
    },
    {
        "name": "wo",
        "multiplicity": 3,
        "geometry": """
            W   0.00000000  0.00000000  0.00000000
            O   0.00000000  0.00000000  1.70000000
        """,
    },
    {
        "name": "hfh2",
        "multiplicity": 3,
        "geometry": """
            Hf  0.00000000  0.00000000 -0.74088040
            H   1.50747855  0.00000000  0.37044020
            H  -1.50747855  0.00000000  0.37044020
        """,
    },
    {
        "name": "hfh4",
        "multiplicity": 1,
        "geometry": """
            Hf  0.00000000  0.00000000  0.00000000
            H   1.06685503 -1.06685503  1.06685503
            H  -1.06685503  1.06685503  1.06685503
            H   1.06685503  1.06685503 -1.06685503
            H  -1.06685503 -1.06685503 -1.06685503
        """,
    },
    {
        "name": "tah2",
        "multiplicity": 4,
        "geometry": """
            Ta  0.00000000  0.00000000 -0.61701204
            H   1.51541391  0.00000000  0.30850602
            H  -1.51541391  0.00000000  0.30850602
        """,
    },
    {
        "name": "tah3",
        "multiplicity": 3,
        "geometry": """
            Ta  0.00000000  0.00000000  0.00000000
            H  -0.89830450 -1.55590904  0.00000000
            H  -0.89830450  1.55590904  0.00000000
            H   1.79660900  0.00000000  0.00000000
        """,
    },
    #
    # W(H2)6, six side-on H2 ligands: the DFT-D3 structure wh2.coord
    # (0 unpaired electrons, coordination number 10.9)
    #
    {
        "name": "wh12",
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
    {"name": "hf", "multiplicity": 3, "geometry": "Hf 0.0 0.0 0.0"},
    {"name": "ta", "multiplicity": 4, "geometry": "Ta 0.0 0.0 0.0"},
    {"name": "w", "multiplicity": 5, "geometry": "W 0.0 0.0 0.0"},
    {"name": "o", "multiplicity": 3, "geometry": "O 0.0 0.0 0.0"},
    {"name": "h", "multiplicity": 2, "geometry": "H 0.0 0.0 0.0"},
]


def solve(mol: gto.Mole, guess: str) -> dict:
    """Converge from one initial guess and follow internal instabilities."""
    start = time.time()
    mean_field = scf.RHF(mol) if mol.spin == 0 else scf.UHF(mol)
    mean_field.init_guess = guess
    mean_field.conv_tol = 1.0e-12
    mean_field.max_cycle = MAX_CYCLE
    mean_field.kernel()
    rounds = 0
    mo, _, stable, _ = mean_field.stability(return_status=True)
    while not stable and rounds < STABILITY_ROUNDS:
        rounds += 1
        mean_field.kernel(mean_field.make_rdm1(mo, mean_field.mo_occ))
        mo, _, stable, _ = mean_field.stability(return_status=True)
    solution = {
        "energy": mean_field.e_tot,
        "converged": mean_field.converged,
        "stable": stable,
        "rounds": rounds,
        "seconds": time.time() - start,
    }
    if mol.spin == 0:
        #
        # beyond-rpa runs closed shells spin-restricted, so an RHF -> UHF
        # instability is only reported
        #
        _, _, _, solution["stable_uhf"] = mean_field.stability(external=True, return_status=True)
    else:
        solution["s2"] = mean_field.spin_square()[0]
    return solution


def describe(solution: dict) -> str:
    status = "stable" if solution["converged"] and solution["stable"] else "NOT CONVERGED OR UNSTABLE"
    text = f"E = {solution['energy']:.10f}  {status}, {solution['rounds']} stability rounds"
    if "s2" in solution:
        text += f", <S^2> = {solution['s2']:.4f}"
    elif not solution["stable_uhf"]:
        text += ", unstable toward UHF"
    return text + f"  ({solution['seconds']:.1f} s)"


def reference(system: dict) -> float:
    """Return the lowest stable energy over the initial guesses."""
    mol = gto.M(
        atom=system["geometry"],
        spin=system["multiplicity"] - 1,
        basis=BASIS,
        ecp=ECP,
        verbose=0,
    )
    energies = []
    for guess in INITIAL_GUESSES:
        try:
            solution = solve(mol, guess)
        except Exception as error:
            print(f"  {guess:<7} failed: {error.__class__.__name__}: {error}", flush=True)
            continue
        print(f"  {guess:<7} {describe(solution)}", flush=True)
        if solution["converged"] and solution["stable"]:
            energies.append((solution["energy"], guess))
    if not energies:
        raise RuntimeError(f"{system['name']}: no converged, stable HF solution")
    energy, guess = min(energies)
    spread = max(e for e, _ in energies) - energy
    print(f"  reference E = {energy:.10f} from {guess}; spread of stable solutions {spread:.1e}", flush=True)
    return energy


def main() -> None:
    blocks = []
    for k, system in enumerate(SPECIES, start=1):
        kind = "RHF" if system["multiplicity"] == 1 else "UHF"
        print(f"[{k}/{len(SPECIES)}] {system['name']}: 2S+1 = {system['multiplicity']}, {kind}", flush=True)
        energy = reference(system)
        blocks.append(
            f"Input: {system['name']}\n"
            f"Multiplicity:               {system['multiplicity']}\n"
            f"HF single point (a.u.):     {energy:.10f}\n"
        )
    OUTPUT.write_text("\n".join(blocks))
    print(f"Wrote {OUTPUT}", flush=True)


if __name__ == "__main__":
    main()
