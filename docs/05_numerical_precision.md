# Control of Numerical Precision

The default thresholds are set for noncovalent interaction energies at the RPA+ph level. Most calculations need no changes. This chapter describes the settings that control numerical precision, for tests and for systems where the defaults are insufficient.

## Accuracy levels

The `accuracy` keyword in the `rpa` block selects a predefined set of thresholds.

| Value | Description |
|---|---|
| `default` | Thresholds for all production calculations, including the many-body expansion of the crystal lattice energy |
| `ludicrous` | Tight thresholds for tests and debugging. Not for production runs. |
| `custom` | Required whenever any parameter of the RPA workflow related to numerical precision is perturbed from its default value. |

## Tensor hypercontraction

Tensor hypercontraction (THC) approximates the Coulomb integrals as

$$(pq|rs) \approx \sum_{gh} X_{gp} X_{gq} Z_{gh} X_{hr} X_{hs}$$

where $X_{gp}$ is the value of basis function $p$ at the THC point $g$, and $Z$ is fitted by least squares to the Cholesky-decomposed integrals [[Parrish2012](04_literature.md)]. The THC points are a subset of a parent molecular grid. A pivoted on-the-fly Cholesky decomposition selects them until the residual drops below a threshold [[Matthews2020](04_literature.md)]. A tighter threshold gives more points, higher accuracy and higher cost.

THC is used in
* the SCF;
* the post-SCF step of `RPA`, `RPA+ph` and `RPA+2g` [[Syty2025](04_literature.md)].

When both steps use THC, the post-SCF step uses a reduced set of points, selected with its own threshold. The post-SCF threshold cannot be tighter than the SCF threshold.
