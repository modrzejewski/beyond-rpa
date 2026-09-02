from pyscf import gto, scf, mp
from pyscf.gw import rpa

def calculate_energies(geometry: str, basis: str, ecp: dict[str, str], frozen_orbitals: int) -> tuple[float, float]:
    """
    Calculate SCF and MP2 correlation energies.
    """
    molecule = gto.M(atom=geometry, basis=basis, ecp=ecp, verbose=3)
    mean_field = scf.RHF(molecule)
    mean_field.conv_tol = 1e-12
    mean_field.direct_scf_tol = 1e-14
    energy_hf = mean_field.kernel()
    rpa_model = rpa.dRPA(mean_field)
    rpa_model.frozen = frozen_orbitals
    rpa_model.with_df.auxbasis = 'def2-universal-jkfit'
    energy_rpa_correlation = rpa_model.kernel(nw=200)
    
    return energy_hf, energy_rpa_correlation

if __name__ == "__main__":
    geometry_ab = """
    Hg  0.000000  0.000000  0.000000
    Hg  0.000000  0.000000  3.630000
    """
    
    geometry_a_ghost_b = """
    Hg    0.000000  0.000000  0.000000
    X-Hg  0.000000  0.000000  3.630000
    """
    
    geometry_b_ghost_a = """
    X-Hg  0.000000  0.000000  0.000000
    Hg    0.000000  0.000000  3.630000
    """
    
    basis = "def2-svpd"
    ecp = {"Hg": "def2-svpd"}
    
    energy_hf_ab, energy_rpa_correlation_ab = calculate_energies(geometry_ab, basis, ecp, 8)
    energy_hf_a, energy_rpa_correlation_a = calculate_energies(geometry_a_ghost_b, basis, ecp, 4)
    energy_hf_b, energy_rpa_correlation_b = calculate_energies(geometry_b_ghost_a, basis, ecp, 4)
    
    interaction_hf_au = energy_hf_ab - energy_hf_a - energy_hf_b
    interaction_rpa_correlation_au = energy_rpa_correlation_ab - energy_rpa_correlation_a - energy_rpa_correlation_b
    
    conversion_factor = 627.5094688043
    interaction_hf_kcal = interaction_hf_au * conversion_factor
    interaction_rpa_correlation_kcal = interaction_rpa_correlation_au * conversion_factor
    
    print(f"HF single point AB (a.u.):         {energy_hf_ab:15.8f}")
    print(f"dRPA single point AB (a.u.):       {energy_rpa_correlation_ab:15.8f}")
    print(f"HF single point A (a.u.):          {energy_hf_a:15.8f}")
    print(f"dRPA single point A (a.u.):        {energy_rpa_correlation_a:15.8f}")
    print(f"HF single point B (a.u.):          {energy_hf_b:15.8f}")
    print(f"dRPA single point B (a.u.):        {energy_rpa_correlation_b:15.8f}")
    print(f"HF interaction A...B (kcal/mol):   {interaction_hf_kcal:15.6f}")
    print(f"dRPA interaction A...B (kcal/mol): {interaction_rpa_correlation_kcal:15.6f}")
