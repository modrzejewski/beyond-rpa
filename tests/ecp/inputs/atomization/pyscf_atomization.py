"""
PySCF reference energies for the atomization tests: HfO, TaO, and WO.

The cc-pVTZ-PP ECPs of Hf, Ta, and W contain r^2 Exp(-a r^2) terms (R^N
exponent 4 in the library files). The test compares the HF atomization
energies of the oxides, assembled from the single points of the molecules and
the atoms. The basis sets are cc-pVTZ-PP on the metals and cc-pVTZ on O. The
built-in PySCF basis sets and ECPs equal bse/cc-pVTZ-PP and cc-repo/cc-pVTZ
of beyond-rpa.

For each input, print the HF single-point energy. Closed shells are RHF, as in
beyond-rpa. Open shells are UHF: the lowest energy over several initial
guesses, each followed by internal stability analysis. The bond length is
1.70 Angstrom for all oxides; it is a test geometry, not the equilibrium.

Usage: python pyscf_atomization.py > pyscf_atomization.txt
Then run inject_reference_preambles.py to update the input files.
Requires pyscf (references were generated with pyscf 2.12.0).
"""
from __future__ import annotations

from pyscf import gto, scf

BOND_LENGTH = 1.70
BASIS = {"Hf": "cc-pvtz-pp", "Ta": "cc-pvtz-pp", "W": "cc-pvtz-pp", "O": "cc-pvtz"}
ECP = {"Hf": "cc-pvtz-pp", "Ta": "cc-pvtz-pp", "W": "cc-pvtz-pp"}
INITIAL_GUESSES = ("minao", "atom", "huckel", "1e")
SPECIES = [
    {"name": "hfo", "geometry": f"Hf 0 0 0; O 0 0 {BOND_LENGTH}", "multiplicity": 1},
    {"name": "tao", "geometry": f"Ta 0 0 0; O 0 0 {BOND_LENGTH}", "multiplicity": 2},
    {"name": "wo", "geometry": f"W 0 0 0; O 0 0 {BOND_LENGTH}", "multiplicity": 3},
    {"name": "hf", "geometry": "Hf 0 0 0", "multiplicity": 3},
    {"name": "ta", "geometry": "Ta 0 0 0", "multiplicity": 4},
    {"name": "w", "geometry": "W 0 0 0", "multiplicity": 5},
    {"name": "o", "geometry": "O 0 0 0", "multiplicity": 3},
]


def stable_energy(mean_field) -> float | None:
    """Converge, follow internal instabilities, and return the energy."""
    mean_field.conv_tol = 1.0e-12
    mean_field.max_cycle = 256
    try:
        mean_field.kernel()
        mo, _, stable, _ = mean_field.stability(return_status=True)
        for _ in range(10):
            if stable:
                break
            mean_field.kernel(mean_field.make_rdm1(mo, mean_field.mo_occ))
            mo, _, stable, _ = mean_field.stability(return_status=True)
    except RuntimeError:
        return None
    return mean_field.e_tot if mean_field.converged and stable else None


def hartree_fock(mol: gto.Mole) -> float:
    """Return the RHF energy of a closed shell, else the lowest UHF energy."""
    energies = []
    for guess in INITIAL_GUESSES:
        mean_field = scf.RHF(mol) if mol.spin == 0 else scf.UHF(mol)
        mean_field.init_guess = guess
        energy = stable_energy(mean_field)
        if energy is not None:
            energies.append(energy)
    if not energies:
        raise RuntimeError(f"no converged, stable HF solution for {mol.atom}")
    return min(energies)


def main() -> None:
    for system in SPECIES:
        mol = gto.M(
            atom=system["geometry"],
            spin=system["multiplicity"] - 1,
            basis=BASIS,
            ecp=ECP,
            verbose=0,
        )
        energy = hartree_fock(mol)
        print(f"Input: {system['name']}")
        print(f"Multiplicity:               {system['multiplicity']}")
        print(f"HF single point (a.u.):     {energy:.10f}")
        print(flush=True)


if __name__ == "__main__":
    main()
