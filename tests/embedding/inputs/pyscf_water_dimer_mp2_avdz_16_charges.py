import numpy as np
from pyscf import gto, scf, qmmm, mp

def calculate_qmmm_energies(geometry: str, basis: str, charge_coordinates: np.ndarray, point_charges: np.ndarray, frozen_orbitals: int) -> tuple[float, float]:
    """
    Calculate SCF and MP2 correlation energies in the presence of point charges.
    """
    molecule = gto.M(atom=geometry, basis=basis, verbose=3)
    mean_field = scf.RHF(molecule)
    mean_field_qmmm = qmmm.mm_charge(mean_field, charge_coordinates, point_charges)
    energy_hf = mean_field_qmmm.kernel()
    mp2_model = mp.MP2(mean_field_qmmm)
    mp2_model.frozen = frozen_orbitals
    energy_mp2_correlation = mp2_model.kernel()[0]
    return energy_hf, energy_mp2_correlation

if __name__ == "__main__":
    geometry_ab = """
    O  1.531750  0.005922 -0.120880
    H  0.575968 -0.005249  0.024966
    H  1.906249 -0.037561  0.763218
    O -1.396226 -0.004990  0.106766
    H -1.789372 -0.742283 -0.371009
    H -1.777037  0.777638 -0.304264
    """
    
    geometry_a_ghost_b = """
    O    1.531750  0.005922 -0.120880
    H    0.575968 -0.005249  0.024966
    H    1.906249 -0.037561  0.763218
    X-O -1.396226 -0.004990  0.106766
    X-H -1.789372 -0.742283 -0.371009
    X-H -1.777037  0.777638 -0.304264
    """
    
    geometry_b_ghost_a = """
    X-O  1.531750  0.005922 -0.120880
    X-H  0.575968 -0.005249  0.024966
    X-H  1.906249 -0.037561  0.763218
    O   -1.396226 -0.004990  0.106766
    H   -1.789372 -0.742283 -0.371009
    H   -1.777037  0.777638 -0.304264
    """
    
    coordinates = np.array([
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
        [ 0.00,  0.00, -6.00]
    ])
    
    charges = np.array([
        1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0,
        1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0
    ]) * (1.0 / 10.0)
    
    basis = "aug-cc-pvdz"
    
    energy_hf_ab, energy_mp2_correlation_ab = calculate_qmmm_energies(geometry_ab, basis, coordinates, charges, 2)
    energy_hf_a, energy_mp2_correlation_a = calculate_qmmm_energies(geometry_a_ghost_b, basis, coordinates, charges, 1)
    energy_hf_b, energy_mp2_correlation_b = calculate_qmmm_energies(geometry_b_ghost_a, basis, coordinates, charges, 1)
    
    interaction_hf_au = energy_hf_ab - energy_hf_a - energy_hf_b
    interaction_mp2_correlation_au = energy_mp2_correlation_ab - energy_mp2_correlation_a - energy_mp2_correlation_b
    
    conversion_factor = 627.5094688043
    interaction_hf_kcal = interaction_hf_au * conversion_factor
    interaction_mp2_correlation_kcal = interaction_mp2_correlation_au * conversion_factor
    
    print("embedding")
    print(len(charges))
    for charge, coordinate in zip(charges, coordinates):
        print(f"Q({charge}) {coordinate[0]} {coordinate[1]} {coordinate[2]}")
    print("end\n")
    
    print(f"reference HF single point energy of AB (in a.u.):              {energy_hf_ab:15.8f}")
    print(f"reference MP2 correlation energy of AB (in a.u.):              {energy_mp2_correlation_ab:15.8f}")
    print(f"reference HF single point energy of A (in a.u.):               {energy_hf_a:15.8f}")
    print(f"reference MP2 correlation energy of A (in a.u.):               {energy_mp2_correlation_a:15.8f}")
    print(f"reference HF single point energy of B (in a.u.):               {energy_hf_b:15.8f}")
    print(f"reference MP2 correlation energy of B (in a.u.):               {energy_mp2_correlation_b:15.8f}")
    print(f"HF interaction energy of A...B (in kcal/mol):                  {interaction_hf_kcal:15.6f}")
    print(f"MP2 correlation interaction energy of A...B (in kcal/mol):     {interaction_mp2_correlation_kcal:15.6f}")
