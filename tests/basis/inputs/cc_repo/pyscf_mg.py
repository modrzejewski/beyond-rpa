"""Reference HF energies of the Mg atom in the ccRepo cc-pwCVXZ basis sets."""
from pyscf import gto, scf

from library_basis import element


def rhf_energy(geometry: str, basis: dict) -> float:
    molecule = gto.M(atom=geometry, basis=basis, verbose=0)
    mean_field = scf.RHF(molecule)
    mean_field.conv_tol = 1e-12
    return mean_field.kernel()


if __name__ == "__main__":
    for name in ("cc-pwcvdz", "cc-pwcvtz", "cc-pwcvqz"):
        basis = {"Mg": element(f"cc-repo/{name}.txt", "MAGNESIUM")}
        energy = rhf_energy("Mg 0.0 0.0 0.0", basis)
        print(f"mg_cc-repo_{name}: {energy:.10f}")
