"""Reference HF energy of MgO: Mg and O in ccRepo cc-pwCVTZ."""
from pyscf import gto, scf

from library_basis import element

if __name__ == "__main__":
    geometry = """
    Mg  0.000000  0.000000  0.000000
    O   0.000000  0.000000  1.749000
    """
    basis = {
        "Mg": element("cc-repo/cc-pwcvtz.txt", "MAGNESIUM"),
        "O": element("cc-repo/cc-pwcvtz.txt", "OXYGEN"),
    }
    molecule = gto.M(atom=geometry, basis=basis, verbose=0)
    mean_field = scf.RHF(molecule)
    mean_field.conv_tol = 1e-12
    energy = mean_field.kernel()
    print(f"mgo_cc-repo_cc-pwcvtz: {energy:.10f}")
