"""Reference HF and MP2 energies for the THC grid tests with semicore Mg orbitals.

The tests check the SG-1 pruning of the THC parent grid. Exact integrals,
no density fitting. Basis sets from the ccRepo files of the beyond-rpa
library. Frozen orbitals per atom as in the FrozenOrbitals keyword:
semicore freezes the 1s orbital of each atom, valence freezes Mg 1s2s2p.

co_mgo: CO on MgO, linear O-Mg-C-O, C and O aug-cc-pVTZ, Mg cc-pwCVTZ
        (semicore) or cc-pVTZ (valence); single points of CO...MgO (AB),
        CO (A), and MgO (B) in the basis of AB, and the counterpoise-corrected
        interaction energies. Geometry: minimum of DF-PBE0-D3(BJ) with C and O
        aug-cc-pVTZ and Mg cc-pwCVTZ.

mgo, so2: single points for the semicore correction of the MP2 correlation
        energy, cc-pwCVXZ with the semicore shell correlated (semicore) and
        cc-pVXZ with the valence shell only (valence), X = T, Q. Geometries:
        minima of PBE0/def2-TZVP.

The output has one block per test case: a line "Input: <name>" followed by
"<quantity> (<unit>): <value>" lines. Single points are in a.u., interaction
energies in kcal/mol, as in the beyond-rpa output.

Usage: python pyscf_semicore_thc.py > pyscf_semicore_thc.txt
Then run inject_reference_preambles.py to update the inputs. The calculation
takes a few minutes.
"""
from pyscf import gto, mp, scf

from library_basis import element

HARTREE_TO_KCAL = 627.5094688043

CO_MGO_GEOMETRY = [
    ("C", 0.0, 0.0, 2.320289, "A"),
    ("O", 0.0, 0.0, 3.440575, "A"),
    ("Mg", 0.0, 0.0, 0.0, "B"),
    ("O", 0.0, 0.0, -1.732093, "B"),
]
NAMES = {"C": "CARBON", "O": "OXYGEN", "Mg": "MAGNESIUM", "S": "SULFUR"}
FROZEN = {
    "semicore": {"C": 1, "O": 1, "Mg": 1},
    "valence": {"C": 1, "O": 1, "Mg": 5},
}
SUBSYSTEMS = ["AB", "A", "B"]
#
# Semicore correction: the heavy atom keeps 1s frozen (semicore) or
# 1s2s2p (valence); O keeps 1s frozen in both.
#
DELTA_GEOMETRIES = {
    "mgo": [
        ("Mg", 0.0, 0.0, 0.0, "A"),
        ("O", 0.0, 0.0, 1.732766, "A"),
    ],
    "so2": [
        ("S", 0.0, 0.0, 0.0, "A"),
        ("O", 1.232592, 0.0, 0.727532, "A"),
        ("O", -1.232592, 0.0, 0.727532, "A"),
    ],
}
DELTA_FROZEN = {
    "mgo": {"semicore": {"Mg": 1, "O": 1}, "valence": {"Mg": 5, "O": 1}},
    "so2": {"semicore": {"S": 1, "O": 1}, "valence": {"S": 5, "O": 1}},
}
DELTA_BASIS = {"semicore": "cc-pwcv{x}", "valence": "cc-pv{x}"}


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


def semicore_delta() -> None:
    for system, atoms in DELTA_GEOMETRIES.items():
        symbols = sorted({symbol for symbol, *_ in atoms})
        for x in ("tz", "qz"):
            for variant, frozen in DELTA_FROZEN[system].items():
                basis = library_basis(
                    {symbol: DELTA_BASIS[variant].format(x=x) for symbol in symbols}
                )
                energy_hf, energy_mp2_correlation = hf_mp2(
                    atoms=atoms,
                    real="A",
                    basis=basis,
                    frozen=frozen,
                )
                print_block(
                    name=f"{system}_{x}_{variant}",
                    entries=[
                        ("Frozen orbitals", str(frozen_orbitals(atoms=atoms, real="A", frozen=frozen))),
                        ("HF single point (a.u.)", f"{energy_hf:.10f}"),
                        ("MP2 correlation (a.u.)", f"{energy_mp2_correlation:.10f}"),
                    ],
                )


if __name__ == "__main__":
    co_mgo()
    semicore_delta()
