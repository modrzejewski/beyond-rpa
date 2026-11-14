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

### The `thc` block

The `thc` block sets the THC parameters. Every keyword is optional. In an SCF-only job, THC is selected with `eri_algorithm thc` in the `scf` block.

| Keyword | Description |
|---|---|
| `QRThresh scf {value}` | Pivot threshold of the points used in the SCF. |
| `QRThresh post-scf {value}` | Pivot threshold of the reduced set of points used after the SCF. |
| `BeckeGridKind {grid}` | Parent molecular grid: `sg1`, `medium`, `fine` or `xfine`, in order of increasing size. |
| `NRadial {n}` | Number of radial shells of the parent grid. If absent, the number of the selected grid is used. |
| `LebedevLmin {order}` | Minimum Lebedev order of every radial shell of the parent grid, given as the degree of the spherical harmonics integrated exactly. If absent, the orders of the selected grid are used. |
| `PhiSquaredThresh {value}` | A point is discarded when every basis function satisfies \|φ(r)\|² ≤ `{value}` there. |
| `BlockDim {n}` | Number of Cholesky vectors per macro iteration of the point selection. |

The `accuracy` levels `default` and `ludicrous` overwrite both `QRThresh` values. Use `accuracy custom` to keep the thresholds given in the `thc` block.

Example with a tightened THC threshold for the SCF step:

```text
rpa
  TheoryLevel RPA+ph
  accuracy custom
end

thc
  QRThresh scf 5.0E-5
  QRThresh post-scf 1.0E-3
end
```
