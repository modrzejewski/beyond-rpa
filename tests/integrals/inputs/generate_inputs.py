"""
Generate the input files of the quadrupole test.

Water is rotated by a generic rotation, so that all six components
of the quadrupole tensor are nonzero. Reference values are computed
with PySCF: RHF, traceless quadrupole (Buckingham definition),
origin at the nuclear charge center.

Usage: python generate_inputs.py
Requires numpy and pyscf (references were generated with pyscf 2.12.0).
The script overwrites the water_tilted_*.inp files in its own directory.
"""

from pathlib import Path
import numpy as np
from pyscf import gto, scf
from pyscf.data import nist

BASES = ["cc-pVDZ", "cc-pVTZ"]
ELEMENTS = ["O", "H", "H"]
COORDS_ANGSTROM = np.array(
    [
        [0.0, 0.0, 0.0],
        [0.0, 0.7573266735, -0.58638126729],
        [0.0, -0.7573266735, -0.58638126729],
    ]
)
ANGLES_DEG = (30.0, 40.0, 50.0)
COMPONENTS = {"xx": (0, 0), "yy": (1, 1), "zz": (2, 2), "xy": (0, 1), "xz": (0, 2), "yz": (1, 2)}
AU2DEBYE = 2.541746473
INPUT_TEMPLATE = """{reference}
jobtype uks sp
basis {basis}

basis_assignment
* {basis}
end

scf
 xcfunc HF
end

xyz
3
{xyz}
end
"""


def rotation_matrix(angles_deg):
    ax, ay, az = np.radians(angles_deg)
    rx = np.array([[1, 0, 0], [0, np.cos(ax), -np.sin(ax)], [0, np.sin(ax), np.cos(ax)]])
    ry = np.array([[np.cos(ay), 0, np.sin(ay)], [0, 1, 0], [-np.sin(ay), 0, np.cos(ay)]])
    rz = np.array([[np.cos(az), -np.sin(az), 0], [np.sin(az), np.cos(az), 0], [0, 0, 1]])
    return rz @ ry @ rx


def traceless_quadrupole(mol, dm):
    coords = mol.atom_coords()
    charges = mol.atom_charges()
    center = charges @ coords / charges.sum()
    with mol.with_common_orig(center):
        rr = mol.intor("int1e_rr").reshape(3, 3, mol.nao, mol.nao)
    q_electronic = -np.einsum("ab,xyab->xy", dm, rr)
    r = coords - center
    q_nuclear = np.einsum("i,ix,iy->xy", charges, r, r)
    q = q_nuclear + q_electronic
    q_traceless = 1.5 * q - 0.5 * np.trace(q) * np.eye(3)
    return q_traceless * AU2DEBYE * nist.BOHR


def main():
    coords = np.round(COORDS_ANGSTROM @ rotation_matrix(ANGLES_DEG).T, 10)
    xyz = "\n".join(
        f"{el:<2} {x:16.10f} {y:16.10f} {z:16.10f}"
        for el, (x, y, z) in zip(ELEMENTS, coords)
    )
    atoms = [(el, tuple(c)) for el, c in zip(ELEMENTS, coords)]
    for basis in BASES:
        mol = gto.M(
            atom=atoms,
            basis=basis,
            unit="Angstrom",
            verbose=0,
        )
        mf = scf.RHF(mol)
        mf.conv_tol = 1.0e-12
        mf.kernel()
        q = traceless_quadrupole(mol, mf.make_rdm1())
        lines = ["! reference from pyscf: traceless quadrupole (Debye*Angs)"]
        for name, (i, j) in COMPONENTS.items():
            lines.append(f"! Q{name} = {q[i, j]:.6f}")
        text = INPUT_TEMPLATE.format(
            reference="\n".join(lines),
            basis=basis,
            xyz=xyz,
        )
        path = Path(__file__).parent / f"water_tilted_{basis}.inp"
        path.write_text(text)


if __name__ == "__main__":
    main()
