"""Reference HF and MP2 energies for the THC grid tests with semicore Mg orbitals.

The tests check the SG-1 pruning of the THC parent grid. Exact integrals,
no density fitting. Basis sets from the ccRepo files of the beyond-rpa
library. Frozen orbitals per atom as in the FrozenOrbitals keyword:
semicore freezes the 1s orbital of each atom, valence freezes Mg 1s2s2p.

mgo:    MgO molecule, Mg and O cc-pwCVTZ
co_mgo: CO on MgO, linear O-Mg-C-O, C and O aug-cc-pVTZ, Mg cc-pwCVTZ
        (semicore) or cc-pVTZ (valence); single points of CO...MgO (AB),
        CO (A), and MgO (B) in the basis of AB, and the counterpoise-corrected
        interaction energies

The output has one block per test case: a line "Input: <name>" followed by
"<quantity> (<unit>): <value>" lines. Single points are in a.u., interaction
energies in kcal/mol, as in the beyond-rpa output.

Usage: python pyscf_semicore_thc.py [mgo] [co_mgo] > pyscf_semicore_thc.txt
Then run inject_reference_preambles.py to update the inputs. The mgo case
takes seconds, the co_mgo case takes minutes.
"""
import sys

from pyscf import gto, mp, scf

from library_basis import element

HARTREE_TO_KCAL = 627.5094688043

MGO_GEOMETRY = [
    ("Mg", 0.0, 0.0, 0.0, "B"),
    ("O", 0.0, 0.0, 1.749, "B"),
]
CO_MGO_GEOMETRY = [
    ("C", 0.0, 0.0, 2.44102235837, "A"),
    ("O", 0.0, 0.0, 3.58784217303, "A"),
    ("Mg", 0.0, 0.0, 0.0, "B"),
    ("O", 0.0, 0.0, -1.749, "B"),
]
NAMES = {"C": "CARBON", "O": "OXYGEN", "Mg": "MAGNESIUM"}
FROZEN = {
    "semicore": {"C": 1, "O": 1, "Mg": 1},
    "valence": {"C": 1, "O": 1, "Mg": 5},
}
SUBSYSTEMS = ["AB", "A", "B"]


def library_basis(
    basis_files: dict[str, str],
) -> dict:
    """PySCF basis of each element from the ccRepo files of the library."""
    return {
        symbol: element(f"cc-repo/{name}.txt", NAMES[symbol])
        for symbol, name in basis_files.items()
    }


def frozen_orbitals(
    atoms: list,
    real: str,
    frozen: dict[str, int],
) -> int:
    """Number of frozen orbitals of the real atoms."""
    return sum(frozen[symbol] for symbol, _, _, _, s in atoms if s in real)


def hf_mp2(
    atoms: list,
    real: str,
    basis: dict,
    frozen: dict[str, int],
) -> tuple[float, float]:
    """RHF energy and frozen-core MP2 correlation energy.

    real: subsystems with real atoms ("AB", "A", "B"); the others are ghosts.
    """
    geometry = "\n".join(
        f"{symbol if s in real else 'ghost:' + symbol} {x} {y} {z}"
        for symbol, x, y, z, s in atoms
    )
    molecule = gto.M(atom=geometry, basis=basis, verbose=0)
    mean_field = scf.RHF(molecule)
    mean_field.conv_tol = 1e-12
    energy_hf = mean_field.kernel()
    if not mean_field.converged:
        raise RuntimeError(f"SCF did not converge for {real}")
    n_frozen = frozen_orbitals(
        atoms=atoms,
        real=real,
        frozen=frozen,
    )
    energy_mp2_correlation = mp.MP2(mean_field, frozen=n_frozen).kernel()[0]
    return energy_hf, energy_mp2_correlation


def print_block(
    name: str,
    entries: list[tuple[str, str]],
) -> None:
    """Print the reference values of one test case."""
    width = max(len(quantity) for quantity, _ in entries) + 2
    print(f"Input: {name}")
    for quantity, value in entries:
        print(f"{quantity + ':':<{width}}{value:>16}")
    print("", flush=True)


def mgo() -> None:
    basis = library_basis({"Mg": "cc-pwcvtz", "O": "cc-pwcvtz"})
    for variant, frozen in FROZEN.items():
        energy_hf, energy_mp2_correlation = hf_mp2(
            atoms=MGO_GEOMETRY,
            real="B",
            basis=basis,
            frozen=frozen,
        )
        n_frozen = frozen_orbitals(
            atoms=MGO_GEOMETRY,
            real="B",
            frozen=frozen,
        )
        print_block(
            name=f"mgo_{variant}",
            entries=[
                ("Frozen orbitals", str(n_frozen)),
                ("HF single point (a.u.)", f"{energy_hf:.10f}"),
                ("MP2 correlation (a.u.)", f"{energy_mp2_correlation:.10f}"),
            ],
        )


def co_mgo() -> None:
    mg_basis = {"semicore": "cc-pwcvtz", "valence": "cc-pvtz"}
    for variant, frozen in FROZEN.items():
        basis = library_basis(
            {"C": "aug-cc-pvtz", "O": "aug-cc-pvtz", "Mg": mg_basis[variant]}
        )
        energies = {
            real: hf_mp2(
                atoms=CO_MGO_GEOMETRY,
                real=real,
                basis=basis,
                frozen=frozen,
            )
            for real in SUBSYSTEMS
        }
        interaction_hf, interaction_mp2_correlation = (
            (energies["AB"][k] - energies["A"][k] - energies["B"][k]) * HARTREE_TO_KCAL
            for k in range(2)
        )
        n_frozen = "/".join(
            str(frozen_orbitals(atoms=CO_MGO_GEOMETRY, real=real, frozen=frozen))
            for real in SUBSYSTEMS
        )
        entries = [("Frozen orbitals AB/A/B", n_frozen)]
        for real in SUBSYSTEMS:
            energy_hf, energy_mp2_correlation = energies[real]
            entries += [
                (f"HF single point {real} (a.u.)", f"{energy_hf:.10f}"),
                (f"MP2 correlation {real} (a.u.)", f"{energy_mp2_correlation:.10f}"),
            ]
        entries += [
            ("HF interaction A...B (kcal/mol)", f"{interaction_hf:.8f}"),
            (
                "MP2 correlation interaction A...B (kcal/mol)",
                f"{interaction_mp2_correlation:.8f}",
            ),
        ]
        print_block(
            name=f"co_mgo_{variant}",
            entries=entries,
        )


if __name__ == "__main__":
    systems = sys.argv[1:] or ["mgo", "co_mgo"]
    for system in systems:
        {"mgo": mgo, "co_mgo": co_mgo}[system]()
