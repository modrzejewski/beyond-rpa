"""
PySCF reference energies for the frozen-core tests: water dimer, cc-pVDZ.

For each frozen-core variant, print the HF and direct RPA (dRPA) correlation
energies of the dimer AB and of the monomers A and B in the dimer basis, and
the interaction energies. The two-electron integrals are decomposed exactly,
so the dRPA energies carry no density-fitting error.

Usage: python pyscf_h2o_h2o_vdz.py > pyscf_h2o_h2o_vdz.txt
Then run inject_reference_preambles.py to update the input files.
Requires numpy, scipy, and pyscf (references were generated with pyscf 2.12.0).
"""

import numpy as np
import scipy.linalg
from pyscf import gto, scf, ao2mo, df
from pyscf.gw import rpa

BASIS = "cc-pvdz"
ECP = {}
ATOMS = [
    ("O", 1.531750, 0.005922, -0.120880),
    ("H", 0.575968, -0.005249, 0.024966),
    ("H", 1.906249, -0.037561, 0.763218),
    ("O", -1.396226, -0.004990, 0.106766),
    ("H", -1.789372, -0.742283, -0.371009),
    ("H", -1.777037, 0.777638, -0.304264),
]
NATOMS_A = 3
#
# Frozen orbitals per element, or "threshold" for the occupied
# orbitals with energies below CORE_ORB_THRESH
#
VARIANTS = [
    ("h2o_h2o_vdz_frozen_O0_H0", {"O": 0, "H": 0}),
    ("h2o_h2o_vdz_frozen_O1_H0", {"O": 1, "H": 0}),
    ("h2o_h2o_vdz_coreorbthresh", "threshold"),
]
CORE_ORB_THRESH = -3.0
HARTREE_TO_KCAL = 627.5094688043


def subsystem_geometry(real_atoms: range) -> str:
    lines = []
    for k, (symbol, x, y, z) in enumerate(ATOMS):
        label = symbol if k in real_atoms else f"X-{symbol}"
        lines.append(f"{label} {x:.6f} {y:.6f} {z:.6f}")
    return "\n".join(lines)


def hartree_fock(geometry: str):
    molecule = gto.M(atom=geometry, basis=BASIS, ecp=ECP, verbose=0)
    mean_field = scf.RHF(molecule)
    mean_field.conv_tol = 1e-12
    mean_field.direct_scf_tol = 1e-14
    mean_field.kernel()
    #
    # Exact 4-center Cholesky decomposition of 2-electron integrals
    #
    eri_s4 = molecule.intor("int2e_sph", aosym="s4")
    eri_s2 = ao2mo.restore(4, eri_s4, molecule.nao)
    w, v = scipy.linalg.eigh(eri_s2)
    idx = w > 1e-12
    cderi = (v[:, idx] * np.sqrt(w[idx])).T
    return molecule, mean_field, cderi


def frozen_orbitals(molecule, mean_field, variant) -> int:
    if variant == "threshold":
        nocc = molecule.nelectron // 2
        return int(np.count_nonzero(mean_field.mo_energy[:nocc] < CORE_ORB_THRESH))
    n = 0
    for a in range(molecule.natm):
        if molecule.atom_charge(a) > 0:
            n += variant[molecule.atom_pure_symbol(a)]
    return n


def drpa_correlation(mean_field, cderi, nfrozen: int) -> float:
    mean_field.with_df = df.DF(mean_field.mol)
    rpa_model = rpa.dRPA(mean_field)
    rpa_model.frozen = nfrozen
    rpa_model.with_df._cderi = cderi
    return rpa_model.kernel(nw=200)


if __name__ == "__main__":
    subsystems = {
        "AB": range(len(ATOMS)),
        "A": range(NATOMS_A),
        "B": range(NATOMS_A, len(ATOMS)),
    }
    models = {key: hartree_fock(subsystem_geometry(atoms)) for key, atoms in subsystems.items()}
    for name, variant in VARIANTS:
        energy_hf = {}
        energy_rpa = {}
        nfrozen = {}
        for key, (molecule, mean_field, cderi) in models.items():
            nfrozen[key] = frozen_orbitals(molecule, mean_field, variant)
            energy_hf[key] = mean_field.e_tot
            energy_rpa[key] = drpa_correlation(mean_field, cderi, nfrozen[key])
        interaction_hf = (energy_hf["AB"] - energy_hf["A"] - energy_hf["B"]) * HARTREE_TO_KCAL
        interaction_rpa = (energy_rpa["AB"] - energy_rpa["A"] - energy_rpa["B"]) * HARTREE_TO_KCAL
        print(f"Input: {name}")
        print(f"{'Frozen orbitals AB/A/B:':<36}{nfrozen['AB']}/{nfrozen['A']}/{nfrozen['B']}")
        for key in ["AB", "A", "B"]:
            print(f"{f'HF single point {key} (a.u.):':<36}{energy_hf[key]:.8f}")
            print(f"{f'dRPA single point {key} (a.u.):':<36}{energy_rpa[key]:.8f}")
        print(f"{'HF interaction A...B (kcal/mol):':<36}{interaction_hf:.6f}")
        print(f"{'dRPA interaction A...B (kcal/mol):':<36}{interaction_rpa:.6f}")
        print("")
