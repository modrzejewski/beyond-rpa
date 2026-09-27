"""
Reference generation for Cd-Hg dimer with embedding Xe ECP centers and point charges.
"""
import numpy as np
import scipy.linalg
from pyscf import gto, scf, ao2mo, qmmm, df
from pyscf.gw import rpa


def calculate_energies(geometry: str, basis: dict, ecp_definition: dict,
                       frozen_orbitals: int,
                       mm_coords: np.ndarray, mm_charges: np.ndarray) -> tuple[float, float]:
    """
    Calculate SCF and dRPA correlation energies in the presence of embedding ECPs and point charges.
    """
    molecule = gto.M(atom=geometry, basis=basis, ecp=ecp_definition, verbose=0)
    mean_field = scf.RHF(molecule)
    mean_field = qmmm.mm_charge(mean_field, mm_coords, mm_charges)

    orig_energy_nuc = molecule.energy_nuc

    def custom_energy_nuc():
        nuc = orig_energy_nuc()
        for j in range(molecule.natm):
            q2 = molecule.atom_charge(j)
            if q2 > 0:
                r = np.linalg.norm(mm_coords - molecule.atom_coord(j), axis=1)
                nuc += q2 * np.sum(mm_charges / r)
        return nuc

    mean_field.energy_nuc = custom_energy_nuc
    mean_field.conv_tol = 1e-12
    mean_field.direct_scf_tol = 1e-14
    energy_hf = mean_field.kernel()

    # Exact 4-center Cholesky decomposition of 2-electron integrals
    eri_s4 = molecule.intor("int2e_sph", aosym="s4")
    eri_s2 = ao2mo.restore(4, eri_s4, molecule.nao)
    w, v = scipy.linalg.eigh(eri_s2)
    idx = w > 1e-12
    cderi = (v[:, idx] * np.sqrt(w[idx])).T

    mean_field.with_df = df.DF(molecule)
    rpa_model = rpa.dRPA(mean_field)
    rpa_model.frozen = frozen_orbitals
    rpa_model.with_df._cderi = cderi
    energy_rpa_correlation = rpa_model.kernel(nw=200)

    return energy_hf, energy_rpa_correlation


if __name__ == "__main__":
    mm_coords = np.array([
        [ 4.12, -3.21,  2.55],
        [-4.55,  2.11, -3.10],
        [ 3.05,  4.44, -2.15],
        [-2.90, -4.80,  1.50],
        [ 5.10,  0.25,  0.85],
        [-5.20, -0.15, -1.25],
        [ 0.85,  5.50,  3.20],
        [-0.75, -5.60, -2.80],
        [ 2.20, -2.30,  4.90],
        [-1.80,  3.40, -4.50],
        [ 4.80,  2.90,  1.10],
        [-3.70, -2.50, -1.70],
        [ 1.50, -4.10,  5.50],
        [-1.10,  5.20, -5.10],
        [ 0.00,  0.00,  6.00],
        [ 0.00,  0.00, -6.00],
    ])

    mm_charges = np.array([
        0.1, -0.1,  0.1, -0.1,
        0.1, -0.1,  0.1, -0.1,
        0.1, -0.1,  0.1, -0.1,
        0.1, -0.1,  0.1, -0.1,
    ])

    ecp_indices = [11, 4, 5, 10]
    ghost_xe_lines = ""
    for idx in ecp_indices:
        c = mm_coords[idx]
        ghost_xe_lines += f"    ghost:Xe {c[0]:9.6f} {c[1]:9.6f} {c[2]:9.6f}\n"

    geometry_ab = f"""
    Cd  0.000000  0.000000 -1.850000
    Hg  0.000000  0.000000  1.850000
{ghost_xe_lines.rstrip()}
    """

    geometry_a_ghost_b = f"""
    Cd        0.000000  0.000000 -1.850000
    ghost:Hg  0.000000  0.000000  1.850000
{ghost_xe_lines.rstrip()}
    """

    geometry_b_ghost_a = f"""
    ghost:Cd  0.000000  0.000000 -1.850000
    Hg        0.000000  0.000000  1.850000
{ghost_xe_lines.rstrip()}
    """

    ecp_raw = gto.basis.load_ecp("def2-ecp", "Xe")
    ecp_definition = {
        "Cd": "def2-svp",
        "Hg": "def2-svp",
        "ghost:Xe": [0, ecp_raw[1]],
    }

    basis = {"Cd": "def2-svp", "Hg": "def2-svp"}

    energy_hf_ab, energy_rpa_correlation_ab = calculate_energies(
        geometry_ab, basis, ecp_definition, 8, mm_coords, mm_charges
    )
    energy_hf_a, energy_rpa_correlation_a = calculate_energies(
        geometry_a_ghost_b, basis, ecp_definition, 4, mm_coords, mm_charges
    )
    energy_hf_b, energy_rpa_correlation_b = calculate_energies(
        geometry_b_ghost_a, basis, ecp_definition, 4, mm_coords, mm_charges
    )

    interaction_hf_au = energy_hf_ab - energy_hf_a - energy_hf_b
    interaction_rpa_correlation_au = (
        energy_rpa_correlation_ab - energy_rpa_correlation_a - energy_rpa_correlation_b
    )

    conversion_factor = 627.5094688043
    interaction_hf_kcal = interaction_hf_au * conversion_factor
    interaction_rpa_correlation_kcal = interaction_rpa_correlation_au * conversion_factor

    print(f"HF single point AB (a.u.):           {energy_hf_ab:15.8f}")
    print(f"dRPA single point AB (a.u.):         {energy_rpa_correlation_ab:15.8f}")
    print(f"HF single point A (a.u.):            {energy_hf_a:15.8f}")
    print(f"dRPA single point A (a.u.):          {energy_rpa_correlation_a:15.8f}")
    print(f"HF single point B (a.u.):            {energy_hf_b:15.8f}")
    print(f"dRPA single point B (a.u.):          {energy_rpa_correlation_b:15.8f}")
    print(f"HF interaction A...B (kcal/mol):     {interaction_hf_kcal:15.6f}")
    print(f"dRPA interaction A...B (kcal/mol):   {interaction_rpa_correlation_kcal:15.6f}")
