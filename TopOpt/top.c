#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <math.h>
#include <cblas.h>

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) > (b) ? (b) : (a))

struct gridContext {
  float E0;
  float Emin;
  float nu;
  float elementSizeX;
  float elementSizeY;
  float elementSizeZ;
  uint_fast32_t nelx; //Num. elems in the x-direciton
  uint_fast32_t nely; //Num. elems. in the y-direction
  uint_fast32_t nelz; //Num. elems. in the z-direction
  float penal; //Penalty factor for power-law approach
};

struct FixedDofs {
  uint_fast32_t n;
  uint_fast32_t *idx;
};

// currently MATRIXPRECISION = CGVECTORPRECISION = double is assumed multiple places in the code..
// if float should be used change here and follow the compiler warnings.
typedef double MATRIXPRECISION;
typedef double CGVECTORPRECISION;

typedef float DTYPE; // design type, for element denseties, gradients and such.

void writeDensityVtkFile(const int nelx, const int nely, const int nelz,
                         const DTYPE *densityArray, const char *filename) {
  int nx = nelx + 1;
  int ny = nely + 1;
  int nz = nelz + 1;

  int numberOfNodes = nx * ny * nz;
  int numberOfElements = nelx * nely * nelz;

  FILE *fid = fopen(filename, "w");

  // write header
  fprintf(fid, "<VTKFile type=\"UnstructuredGrid\" version=\"0.1\" "
               "byte_order=\"LittleEndian\">\n");
  fprintf(fid, "<UnstructuredGrid>\n");
  fprintf(fid, "<Piece NumberOfPoints=\"%i\" NumberOfCells=\"%i\">\n",
          numberOfNodes, numberOfElements);

  // points
  fprintf(fid, "<Points>\n");
  fprintf(fid,
          "<DataArray type=\"Float32\" NumberOfComponents=\"%i\" "
          "format=\"ascii\">\n",
          3);
  for (int i = 0; i < nx; i++)
    for (int k = 0; k < nz; k++)
      for (int j = 0; j < ny; j++)
        fprintf(fid, "%e %e %e\n", (float)i, (float)j, (float)k);
  fprintf(fid, "</DataArray>\n");
  fprintf(fid, "</Points>\n");

  fprintf(fid, "<Cells>\n");

  fprintf(
      fid,
      "<DataArray type=\"Int32\" Name=\"connectivity\" format= \"ascii\">\n");
  for (int i = 0; i < nelx; i++)
    for (int k = 0; k < nelz; k++)
      for (int j = 0; j < nely; j++) {
        const int nx_1 = i;
        const int nx_2 = i + 1;
        const int nz_1 = k;
        const int nz_2 = k + 1;
        const int ny_1 = j;
        const int ny_2 = j + 1;
        fprintf(fid, "%d %d %d %d %d %d %d %d\n",
                nx_1 * ny * nz + nz_1 * ny + ny_2,
                nx_2 * ny * nz + nz_1 * ny + ny_2,
                nx_2 * ny * nz + nz_1 * ny + ny_1,
                nx_1 * ny * nz + nz_1 * ny + ny_1,
                nx_1 * ny * nz + nz_2 * ny + ny_2,
                nx_2 * ny * nz + nz_2 * ny + ny_2,
                nx_2 * ny * nz + nz_2 * ny + ny_1,
                nx_1 * ny * nz + nz_2 * ny + ny_1);
      }

  fprintf(fid, "</DataArray>\n");

  fprintf(fid,
          "<DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\">\n");
  for (int i = 1; i < numberOfElements + 1; i++)
    fprintf(fid, "%d\n", i * 8);
  fprintf(fid, "</DataArray>\n");

  fprintf(fid, "<DataArray type=\"UInt8\" Name=\"types\" format=\"ascii\">\n");
  for (int i = 0; i < numberOfElements; i++)
    fprintf(fid, "%d\n", 12);
  fprintf(fid, "</DataArray>\n");
  fprintf(fid, "</Cells>\n");

  fprintf(fid, "<CellData>\n");
  fprintf(fid, "<DataArray type=\"Float32\" NumberOfComponents=\"1\" "
               "Name=\"density\" format=\"ascii\">\n");
  for (int i = 0; i < numberOfElements; i++)
    fprintf(fid, "%e\n", densityArray[i]);
  fprintf(fid, "</DataArray>\n");
  fprintf(fid, "</CellData>\n");

  fprintf(fid, "</Piece>\n");
  fprintf(fid, "</UnstructuredGrid>\n");
  fprintf(fid, "</VTKFile>\n");

  fclose(fid);
}


// this function acts as a matrix-free replacement for out = (H*rho(:))./Hs
// note that rho and out cannot be the same pointer!
/**
 * @brief this function acts as a matrix-free replacement for out = (H*rho(:))./Hs
 * note that rho and out cannot be the same pointer!
 * In other words: Corresponds to eq. (5). Calculates the Hf-value for a neighbourhood,
 * and uses this value to update estimate of dc/dx_e
 * 
 * @param gc The gridcontext of the current problem
 * @param rmin Minimum radius in which to operate on the neighbourhood
 * @param rho Current estimates of (x_e * dc/dx_e), stored in x-vector
 * @param out Updated estimates after filtering
 */
void applyDensityFilter(const struct gridContext gc, const float rmin,
                        const float *rho, float *out) {

  const uint32_t nelx = gc.nelx;
  const uint32_t nely = gc.nely;
  const uint32_t nelz = gc.nelz;

  // loop over elementss
  for (unsigned int i1 = 0; i1 < nelx; i1++)
    for (unsigned int k1 = 0; k1 < nelz; k1++)
      for (unsigned int j1 = 0; j1 < nely; j1++) {

        const uint64_t e1 = i1 * nely * nelz + k1 * nely + j1;

        out[e1] = 0.0;
        float unityScale = 0.0;

        // loop over neighbourhood
        const uint32_t i2max = MIN(i1 + (ceil(rmin) + 1), nelx);
        const uint32_t i2min = MAX(i1 - (ceil(rmin) - 1), 0);

        for (uint32_t i2 = i2min; i2 < i2max; i2++) {

          const uint32_t k2max = MIN(k1 + (ceil(rmin) + 1), nelz);
          const uint32_t k2min = MAX(k1 - (ceil(rmin) - 1), 0);

          for (uint32_t k2 = k2min; k2 < k2max; k2++) {

            const uint32_t j2max = MIN(j1 + (ceil(rmin) + 1), nely);
            const uint32_t j2min = MAX(j1 - (ceil(rmin) - 1), 0);

            for (uint32_t j2 = j2min; j2 < j2max; j2++) {

              const uint64_t e2 = i2 * nely * nelz + k2 * nely + j2; //Index of neighbouring element in global indices

              const float filterWeight = //Corresponds to Hf
                  MAX(0.0, rmin - sqrt((i1 - i2) * (i1 - i2) +
                                       (j1 - j2) * (j1 - j2) +
                                       (k1 - k2) * (k1 - k2)));

              out[e1] += filterWeight * rho[e2]; //rho=x-matrix, contains dc/dx_f
              unityScale += filterWeight; //Sum values of Hf in neighbourhood
            }
          }
        }

        out[e1] /= unityScale;
      }
}

// 
/**
 * @brief this function acts as a matrix-free replacement for v = H* (v(:)./Hs)
 * note that rho and out cannot be the same pointer!
 * In other words: First performs an elementwise division over all values in v with Hs (Hf?),
 * and then performs the matrix-vector product of H and the scaled vector
 * I think this also corresponds to eq (5) in some way
 * 
 * @param gc The grid context of the current problem
 * @param rmin The radius defining which neighbours to operate on
 * @param v The vector of values to operate on
 */
void applyDensityFilterGradient(const struct gridContext gc, const float rmin,
                                float *v) {
  const uint32_t nelx = gc.nelx;
  const uint32_t nely = gc.nely;
  const uint32_t nelz = gc.nelz;
  float *tmp = malloc(sizeof(float) * nelx * nely * nelz);

  // loop over elementss
  for (unsigned int i1 = 0; i1 < nelx; i1++)
    for (unsigned int k1 = 0; k1 < nelz; k1++)
      for (unsigned int j1 = 0; j1 < nely; j1++) {

        const uint64_t e1 = i1 * nely * nelz + k1 * nely + j1; //global index in our vector, indicating the object we will be operating on

        float unityScale = 0.0; //Corresponds to sum(H_f)

        // loop over neighbourhood
        const uint32_t i2max = MIN(i1 + (ceil(rmin) + 1), nelx); //Uses MIN to make sure we don't check cells that are out of bounds
        const uint32_t i2min = MAX(i1 - (ceil(rmin) - 1), 0); //Uses MAX to make sure we don't check cells out of bounds

        for (uint32_t i2 = i2min; i2 < i2max; i2++) {

			 //Like above
          const uint32_t k2max = MIN(k1 + (ceil(rmin) + 1), nelz);
          const uint32_t k2min = MAX(k1 - (ceil(rmin) - 1), 0);

          for (uint32_t k2 = k2min; k2 < k2max; k2++) {

				//Like above
            const uint32_t j2max = MIN(j1 + (ceil(rmin) + 1), nely);
            const uint32_t j2min = MAX(j1 - (ceil(rmin) - 1), 0);

            for (uint32_t j2 = j2min; j2 < j2max; j2++) {

				  //Element index 2. The global index of the neighbourhood element we're inspecting
              const uint64_t e2 = i2 * nely * nelz + k2 * nely + j2;

					//For each element, add Hf=rmin-dist(e,f)
              const float filterWeight = //Corresponds to Hf
                  MAX(0.0, rmin - sqrt((i1 - i2) * (i1 - i2) +
                                       (j1 - j2) * (j1 - j2) +
                                       (k1 - k2) * (k1 - k2)));

              unityScale += filterWeight; //Sum values of Hf in neighbourhood
            }
          }
        }

			//unityScale = sum(Hf)
			//v[e1] may be the value dc/dx
        tmp[e1] = v[e1] / unityScale;
      }

  // loop over elementss
  for (unsigned int i1 = 0; i1 < nelx; i1++)
    for (unsigned int k1 = 0; k1 < nelz; k1++)
      for (unsigned int j1 = 0; j1 < nely; j1++) {

        const uint64_t e1 = i1 * nely * nelz + k1 * nely + j1;

        v[e1] = 0.0;

        // loop over neighbourhood
        const uint32_t i2max = MIN(i1 + (ceil(rmin) + 1), nelx);
        const uint32_t i2min = MAX(i1 - (ceil(rmin) - 1), 0);

        for (uint32_t i2 = i2min; i2 < i2max; i2++) {

          const uint32_t k2max = MIN(k1 + (ceil(rmin) + 1), nelz);
          const uint32_t k2min = MAX(k1 - (ceil(rmin) - 1), 0);

          for (uint32_t k2 = k2min; k2 < k2max; k2++) {

            const uint32_t j2max = MIN(j1 + (ceil(rmin) + 1), nely);
            const uint32_t j2min = MAX(j1 - (ceil(rmin) - 1), 0);

            for (uint32_t j2 = j2min; j2 < j2max; j2++) {

              const uint64_t e2 = i2 * nely * nelz + k2 * nely + j2;

              const float filterWeight =
                  MAX(0.0, rmin - sqrt((i1 - i2) * (i1 - i2) +
                                       (j1 - j2) * (j1 - j2) +
                                       (k1 - k2) * (k1 - k2)));

              v[e1] += filterWeight * tmp[e2];
            }
          }
        }
      }

  free(tmp);
}

void getC(MATRIXPRECISION C[6][6],  /* out */
          const MATRIXPRECISION nu) /*  in */
{
  const MATRIXPRECISION temp1 = (1.0 - nu) / ((1.0 + nu) * (1.0 - 2.0 * nu));
  const MATRIXPRECISION temp2 = nu / ((1.0 + nu) * (1.0 - 2.0 * nu));
  const MATRIXPRECISION temp3 = 1.0 / (2.0 * (1.0 + nu));

  for (unsigned int i = 0; i < 6 * 6; i++)
    *((MATRIXPRECISION *)C + i) = 0.0;

  C[0][0] = temp1;
  C[1][1] = temp1;
  C[2][2] = temp1;
  C[3][3] = temp3;
  C[4][4] = temp3;
  C[5][5] = temp3;
  C[0][1] = temp2;
  C[1][0] = temp2;
  C[0][2] = temp2;
  C[2][0] = temp2;
  C[1][2] = temp2;
  C[2][1] = temp2;
}

void getB(MATRIXPRECISION B[6][24],     /* out */
          MATRIXPRECISION *jdet,        /* out */
          const MATRIXPRECISION iso[3], /*  in */
          const MATRIXPRECISION xe[24]) /*  in */
{
  /*     xi = iso(1); */
  const MATRIXPRECISION xi = iso[1 - 1];
  /*     eta = iso(2); */
  const MATRIXPRECISION eta = iso[2 - 1];
  /*     zeta = iso(3); */
  const MATRIXPRECISION zeta = iso[3 - 1];

  const MATRIXPRECISION n1xi = -0.125 * (1 - eta) * (1 - zeta);
  const MATRIXPRECISION n1eta = -0.125 * (1 - xi) * (1 - zeta);
  const MATRIXPRECISION n1zeta = -0.125 * (1 - xi) * (1 - eta);
  const MATRIXPRECISION n2xi = 0.125 * (1 - eta) * (1 - zeta);
  const MATRIXPRECISION n2eta = -0.125 * (1 + xi) * (1 - zeta);
  const MATRIXPRECISION n2zeta = -0.125 * (1 + xi) * (1 - eta);

  const MATRIXPRECISION n3xi = 0.125 * (1 + eta) * (1 - zeta);
  const MATRIXPRECISION n3eta = 0.125 * (1 + xi) * (1 - zeta);
  const MATRIXPRECISION n3zeta = -0.125 * (1 + xi) * (1 + eta);
  const MATRIXPRECISION n4xi = -0.125 * (1 + eta) * (1 - zeta);
  const MATRIXPRECISION n4eta = 0.125 * (1 - xi) * (1 - zeta);
  const MATRIXPRECISION n4zeta = -0.125 * (1 - xi) * (1 + eta);

  const MATRIXPRECISION n5xi = -0.125 * (1 - eta) * (1 + zeta);
  const MATRIXPRECISION n5eta = -0.125 * (1 - xi) * (1 + zeta);
  const MATRIXPRECISION n5zeta = 0.125 * (1 - xi) * (1 - eta);
  const MATRIXPRECISION n6xi = 0.125 * (1 - eta) * (1 + zeta);
  const MATRIXPRECISION n6eta = -0.125 * (1 + xi) * (1 + zeta);
  const MATRIXPRECISION n6zeta = 0.125 * (1 + xi) * (1 - eta);

  const MATRIXPRECISION n7xi = 0.125 * (1 + eta) * (1 + zeta);
  const MATRIXPRECISION n7eta = 0.125 * (1 + xi) * (1 + zeta);
  const MATRIXPRECISION n7zeta = 0.125 * (1 + xi) * (1 + eta);
  const MATRIXPRECISION n8xi = -0.125 * (1 + eta) * (1 + zeta);
  const MATRIXPRECISION n8eta = 0.125 * (1 - xi) * (1 + zeta);
  const MATRIXPRECISION n8zeta = 0.125 * (1 - xi) * (1 + eta);

  /*     L = zeros(6,9); */
  MATRIXPRECISION L[6][9];

  for (unsigned int i = 0; i < 6 * 9; i++)
    *((MATRIXPRECISION *)L + i) = 0.0;

  /*     jac = zeros(3); */
  MATRIXPRECISION jac[3][3];

  for (unsigned int i = 0; i < 3 * 3; i++)
    *((MATRIXPRECISION *)jac + i) = 0.0;

  /*     jacinvt = zeros(9); */
  MATRIXPRECISION jacinvt[9][9];

  for (unsigned int i = 0; i < 9 * 9; i++)
    *((MATRIXPRECISION *)jacinvt + i) = 0.0;

  /*     Nt = zeros(9,24); */
  MATRIXPRECISION Nt[9][24];

  for (unsigned int i = 0; i < 9 * 24; i++)
    *((MATRIXPRECISION *)Nt + i) = 0.0;

	//Magic numbers! Why these specific fields and values? -K
  L[0][0] = 1.0;
  L[1][4] = 1.0;
  L[2][8] = 1.0;
  L[3][1] = 1.0;
  L[3][3] = 1.0;
  L[4][5] = 1.0;
  L[4][7] = 1.0;
  L[5][2] = 1.0;
  L[5][6] = 1.0;

  Nt[0][0] = n1xi;
  Nt[1][0] = n1eta;
  Nt[2][0] = n1zeta;
  Nt[0][3] = n2xi;
  Nt[1][3] = n2eta;
  Nt[2][3] = n2zeta;
  Nt[0][6] = n3xi;
  Nt[1][6] = n3eta;
  Nt[2][6] = n3zeta;
  Nt[0][9] = n4xi;
  Nt[1][9] = n4eta;
  Nt[2][9] = n4zeta;
  Nt[0][12] = n5xi;
  Nt[1][12] = n5eta;
  Nt[2][12] = n5zeta;
  Nt[0][15] = n6xi;
  Nt[1][15] = n6eta;
  Nt[2][15] = n6zeta;
  Nt[0][18] = n7xi;
  Nt[1][18] = n7eta;
  Nt[2][18] = n7zeta;
  Nt[0][21] = n8xi;
  Nt[1][21] = n8eta;
  Nt[2][21] = n8zeta;

  Nt[3][1] = n1xi;
  Nt[4][1] = n1eta;
  Nt[5][1] = n1zeta;
  Nt[3][4] = n2xi;
  Nt[4][4] = n2eta;
  Nt[5][4] = n2zeta;
  Nt[3][7] = n3xi;
  Nt[4][7] = n3eta;
  Nt[5][7] = n3zeta;
  Nt[3][10] = n4xi;
  Nt[4][10] = n4eta;
  Nt[5][10] = n4zeta;
  Nt[3][13] = n5xi;
  Nt[4][13] = n5eta;
  Nt[5][13] = n5zeta;
  Nt[3][16] = n6xi;
  Nt[4][16] = n6eta;
  Nt[5][16] = n6zeta;
  Nt[3][19] = n7xi;
  Nt[4][19] = n7eta;
  Nt[5][19] = n7zeta;
  Nt[3][22] = n8xi;
  Nt[4][22] = n8eta;
  Nt[5][22] = n8zeta;

  Nt[6][2] = n1xi;
  Nt[7][2] = n1eta;
  Nt[8][2] = n1zeta;
  Nt[6][5] = n2xi;
  Nt[7][5] = n2eta;
  Nt[8][5] = n2zeta;
  Nt[6][8] = n3xi;
  Nt[7][8] = n3eta;
  Nt[8][8] = n3zeta;
  Nt[6][11] = n4xi;
  Nt[7][11] = n4eta;
  Nt[8][11] = n4zeta;
  Nt[6][14] = n5xi;
  Nt[7][14] = n5eta;
  Nt[8][14] = n5zeta;
  Nt[6][17] = n6xi;
  Nt[7][17] = n6eta;
  Nt[8][17] = n6zeta;
  Nt[6][20] = n7xi;
  Nt[7][20] = n7eta;
  Nt[8][20] = n7zeta;
  Nt[6][23] = n8xi;
  Nt[7][23] = n8eta;
  Nt[8][23] = n8zeta;

  jac[0][0] = n1xi * xe[0] + n2xi * xe[3] + n3xi * xe[6] + n4xi * xe[9] +
              n5xi * xe[12] + n6xi * xe[15] + n7xi * xe[18] + n8xi * xe[21];

  jac[1][0] = n1eta * xe[0] + n2eta * xe[3] + n3eta * xe[6] + n4eta * xe[9] +
              n5eta * xe[12] + n6eta * xe[15] + n7eta * xe[18] + n8eta * xe[21];

  jac[2][0] = n1zeta * xe[0] + n2zeta * xe[3] + n3zeta * xe[6] +
              n4zeta * xe[9] + n5zeta * xe[12] + n6zeta * xe[15] +
              n7zeta * xe[18] + n8zeta * xe[21];

  jac[0][1] = n1xi * xe[1] + n2xi * xe[4] + n3xi * xe[7] + n4xi * xe[10] +
              n5xi * xe[13] + n6xi * xe[16] + n7xi * xe[19] + n8xi * xe[22];

  jac[1][1] = n1eta * xe[1] + n2eta * xe[4] + n3eta * xe[7] + n4eta * xe[10] +
              n5eta * xe[13] + n6eta * xe[16] + n7eta * xe[19] + n8eta * xe[22];

  jac[2][1] = n1zeta * xe[1] + n2zeta * xe[4] + n3zeta * xe[7] +
              n4zeta * xe[10] + n5zeta * xe[13] + n6zeta * xe[16] +
              n7zeta * xe[19] + n8zeta * xe[22];

  jac[0][2] = n1xi * xe[2] + n2xi * xe[5] + n3xi * xe[8] + n4xi * xe[11] +
              n5xi * xe[14] + n6xi * xe[17] + n7xi * xe[20] + n8xi * xe[23];

  jac[1][2] = n1eta * xe[2] + n2eta * xe[5] + n3eta * xe[8] + n4eta * xe[11] +
              n5eta * xe[14] + n6eta * xe[17] + n7eta * xe[20] + n8eta * xe[23];

  jac[2][2] = n1zeta * xe[2] + n2zeta * xe[5] + n3zeta * xe[8] +
              n4zeta * xe[11] + n5zeta * xe[14] + n6zeta * xe[17] +
              n7zeta * xe[20] + n8zeta * xe[23];

  (*jdet) = jac[0][0] * (jac[1][1] * jac[2][2] - jac[1][2] * jac[1][2]) -
            jac[0][1] * (jac[1][0] * jac[2][2] - jac[1][2] * jac[2][0]) +
            jac[0][2] * (jac[1][0] * jac[2][1] - jac[1][1] * jac[2][0]);

  /*     ijac = inv(jac); */
  // https://en.wikipedia.org/wiki/Invertible_matrix#Inversion_of_3_%C3%97_3_matrices
  jacinvt[0][0] =
      (1.0 / (*jdet)) * (jac[1][1] * jac[2][2] - jac[1][2] * jac[1][2]);
  jacinvt[0][1] =
      (1.0 / (*jdet)) * -1.0 * (jac[1][0] * jac[2][2] - jac[1][2] * jac[2][0]);
  jacinvt[0][2] =
      (1.0 / (*jdet)) * (jac[1][0] * jac[2][1] - jac[1][1] * jac[2][0]);

  jacinvt[1][0] =
      (1.0 / (*jdet)) * -1.0 * (jac[0][1] * jac[2][2] - jac[0][2] * jac[2][1]);
  jacinvt[1][1] =
      (1.0 / (*jdet)) * (jac[0][0] * jac[2][2] - jac[0][2] * jac[2][0]);
  jacinvt[1][2] =
      (1.0 / (*jdet)) * -1.0 * (jac[0][0] * jac[2][1] - jac[0][1] * jac[2][0]);

  jacinvt[2][0] =
      (1.0 / (*jdet)) * (jac[0][1] * jac[1][2] - jac[0][2] * jac[1][1]);
  jacinvt[2][1] =
      (1.0 / (*jdet)) * -1.0 * (jac[0][0] * jac[1][2] - jac[0][2] * jac[1][0]);
  jacinvt[2][2] =
      (1.0 / (*jdet)) * (jac[0][0] * jac[1][1] - jac[0][1] * jac[1][0]);

  for (int k = 1; k < 3; k++)
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++)
        jacinvt[3 * k + i][3 * k + j] = jacinvt[i][j];

  /*     B = (L * jacinvt * Nt); */
  MATRIXPRECISION partial[9][24];

  /*! \todo add checks so that double / single precision GEMM
    is selected. */
  cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, 9, 24, 9, 1.0,
              (MATRIXPRECISION *)jacinvt, 9, (MATRIXPRECISION *)Nt, 24, 0.0,
              (MATRIXPRECISION *)partial, 24);

  cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, 6, 24, 9, 1.0,
              (MATRIXPRECISION *)L, 9, (MATRIXPRECISION *)partial, 24, 0.0,
              (MATRIXPRECISION *)B, 24);
}

// precompute the local matrix for an element
void getKE(MATRIXPRECISION KE[24][24], /* out */
           const MATRIXPRECISION nu,   /* in */
           const MATRIXPRECISION a,    /* in */
           const MATRIXPRECISION b,    /* in */
           const MATRIXPRECISION c)    /* in */
{

  const MATRIXPRECISION xe[24] = {-a, -b, -c, a, -b, -c, a, b, -c, -a, b, -c,
                                  -a, -b, c,  a, -b, c,  a, b, c,  -a, b, c};

  MATRIXPRECISION C[6][6];
  getC(C, nu);

  MATRIXPRECISION qp[2] = {1.0 / sqrt(3.0), -1.0 / sqrt(3.0)};
  MATRIXPRECISION iso[3];
  MATRIXPRECISION B[6][24];
  MATRIXPRECISION partial[24][6];
  MATRIXPRECISION jdet;

  for (unsigned int i = 0; i < 24 * 24; i++)
    *((MATRIXPRECISION *)KE + i) = 0.0;

  for (int i = 0; i < 2; i++)
    for (int j = 0; j < 2; j++)
      for (int k = 0; k < 2; k++) {

        iso[0] = qp[i];
        iso[1] = qp[j];
        iso[2] = qp[k];
        getB(B, &jdet, iso, xe);

        /*! \todo add checks so that double / single precision GEMM
          is selected. */
        cblas_dgemm(CblasRowMajor, CblasTrans, CblasNoTrans, 24, 6, 6, 1.0,
                    (MATRIXPRECISION *)B, 24, (MATRIXPRECISION *)C, 6, 0.0,
                    (MATRIXPRECISION *)partial, 6);

        cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, 24, 24, 6, jdet,
                    (MATRIXPRECISION *)partial, 6, (MATRIXPRECISION *)B, 24,
                    1.0, (MATRIXPRECISION *)KE, 24);
      }
}

// this is were boundary condtions are defined for the grid.
struct FixedDofs getFixedDof(int nelx, int nely, int nelz) {
  struct FixedDofs fd;
  fd.n = 3 * (nely + 1) * (nelz + 1);
  fd.idx = malloc(sizeof(uint_fast32_t) * fd.n);
  for (uint_fast32_t i = 0; i < fd.n; i++)
    fd.idx[i] = i;
  return fd;
}

// compute the indices assosiated with state of element (i,j,k)
/**
 * @brief Compute the indices of the of the 24 degrees of freedom associated with the 8 corners of the element at (i,j,k) in the grid
 * 
 * @param edof output where the indices are stored. edof[0-2] are (x,y,z) degrees for first corner, edof[3-5] for second corner etc
 * @param i Current iteration/coordinate in the x-direction
 * @param j Current iteration/coordinate in the y-direction
 * @param k Current iteration/coordinate in the z-direction
 * @param ny Number of nodes in the y-direction of the grid
 * @param nx Number of nodes in the x-direction of the grid
 */
void getEdof(uint_fast32_t edof[24] /* out */, 
	const int i, const int j, const int k,
	const int ny, const int nz) {

  const int nx_1 = i;
  const int nx_2 = i + 1;
  const int nz_1 = k;
  const int nz_2 = k + 1;
  const int ny_1 = j;
  const int ny_2 = j + 1;

	//nIndex1 is the index for first corner, nIndex2 is the index of second corner, etc?
	//for each index, we extract 3 values (dof in x,y,z direction)
	//For the below, assuming a 3x3x3 element grid and i,j,k=0,0,0
	//index1=0+0+1=1,		index2=16+0+1=17,		index3=16+0+0=16,		index4=0+0+0=0,
	//index5=0+4+1=5,		index6=16+4+1=21,		index7=16+4+0=20,		index8=0+4+0=4		
  const uint_fast32_t nIndex1 = nx_1 * ny * nz + nz_1 * ny + ny_2;
  const uint_fast32_t nIndex2 = nx_2 * ny * nz + nz_1 * ny + ny_2;
  const uint_fast32_t nIndex3 = nx_2 * ny * nz + nz_1 * ny + ny_1;
  const uint_fast32_t nIndex4 = nx_1 * ny * nz + nz_1 * ny + ny_1;
  const uint_fast32_t nIndex5 = nx_1 * ny * nz + nz_2 * ny + ny_2;
  const uint_fast32_t nIndex6 = nx_2 * ny * nz + nz_2 * ny + ny_2;
  const uint_fast32_t nIndex7 = nx_2 * ny * nz + nz_2 * ny + ny_1;
  const uint_fast32_t nIndex8 = nx_1 * ny * nz + nz_2 * ny + ny_1;

  edof[0] = 3 * nIndex1 + 0;
  edof[1] = 3 * nIndex1 + 1;
  edof[2] = 3 * nIndex1 + 2;
  edof[3] = 3 * nIndex2 + 0;
  edof[4] = 3 * nIndex2 + 1;
  edof[5] = 3 * nIndex2 + 2;
  edof[6] = 3 * nIndex3 + 0;
  edof[7] = 3 * nIndex3 + 1;
  edof[8] = 3 * nIndex3 + 2;
  edof[9] = 3 * nIndex4 + 0;
  edof[10] = 3 * nIndex4 + 1;
  edof[11] = 3 * nIndex4 + 2;

  edof[12] = 3 * nIndex5 + 0;
  edof[13] = 3 * nIndex5 + 1;
  edof[14] = 3 * nIndex5 + 2;
  edof[15] = 3 * nIndex6 + 0;
  edof[16] = 3 * nIndex6 + 1;
  edof[17] = 3 * nIndex6 + 2;
  edof[18] = 3 * nIndex7 + 0;
  edof[19] = 3 * nIndex7 + 1;
  edof[20] = 3 * nIndex7 + 2;
  edof[21] = 3 * nIndex8 + 0;
  edof[22] = 3 * nIndex8 + 1;
  edof[23] = 3 * nIndex8 + 2;
}

/* Computes the matrix-vector product out = K*in */
void applyStateOperator(const struct gridContext gc /* in */, float *x /* in: element denseties */,
                        MATRIXPRECISION *in /* in: matrix to be multiplied */, MATRIXPRECISION *out /* out: result of multiplication */) {

  uint_fast32_t edof[24];

  /* Note that the call to getKE is not free, and it could be computed offline, as KE is compile-time constant */
  MATRIXPRECISION ke[24][24];
  MATRIXPRECISION u_local[24];
  MATRIXPRECISION out_local[24];
  getKE(ke, gc.nu, gc.elementSizeX, gc.elementSizeY, gc.elementSizeZ);

  const int ny = gc.nely + 1;
  const int nz = gc.nelz + 1;

  const uint_fast32_t ndof = 3 * ny * nz * (gc.nelx + 1);
  for (unsigned int i = 0; i < ndof; i++)
    *((MATRIXPRECISION *)out + i) = 0.0;

  // I see two ways of doing this. two copies and a level 2 BLAS or our own loop
  // working direcly on the input/output memory -E
  for (int32_t i = 0; i < gc.nelx; i++)
    for (int32_t k = 0; k < gc.nelz; k++)
      for (int32_t j = 0; j < gc.nely; j++) {

        getEdof(edof, i, j, k, ny, nz);

        const uint_fast32_t elementIndex =
            i * gc.nely * gc.nelz + k * gc.nely + j;
        const MATRIXPRECISION elementScale =
            gc.Emin + pow(x[elementIndex], gc.penal) * (gc.E0 - gc.Emin);

        for (int ii = 0; ii < 24; ii++)
          u_local[ii] = in[edof[ii]];

        // matrix-vector product: out_local = (elementScale*ke) * u_local
        cblas_dsymv(CblasRowMajor, CblasUpper, 24, elementScale,
                    (MATRIXPRECISION *)ke, 24, u_local, 1, 0.0, out_local, 1);

        for (int ii = 0; ii < 24; ii++)
          out[edof[ii]] += out_local[ii];
      }

  // apply boundaryConditions
  struct FixedDofs fd = getFixedDof(gc.nelx, gc.nely, gc.nelz);
  for (int i = 0; i < fd.n; i++)
    out[fd.idx[i]] = in[fd.idx[i]];

  free(fd.idx);
}

/**
 * @brief Gets the matrix diagonal of the KE matrix
 * 
 */
void generateMatrixDiagonal(const struct gridContext gc /* in */, DTYPE *x /* in: element denseties */,
                            MATRIXPRECISION *diag /* in/out: preallocated array for output/diagonal of matrix */) {

  uint_fast32_t edof[24];

  /* Note that the call to getKE is not free, and it could be computed offline, as KE is compile-time constant */
  MATRIXPRECISION ke[24][24];
  MATRIXPRECISION diag_local[24];
  getKE(ke, gc.nu, gc.elementSizeX, gc.elementSizeY, gc.elementSizeZ);

  const int ny = gc.nely + 1;
  const int nz = gc.nelz + 1;

  const uint_fast32_t ndof = 3 * ny * nz * (gc.nelx + 1);
  for (unsigned int i = 0; i < ndof; i++)
    *((MATRIXPRECISION *)diag + i) = 0.0;

  // I see two ways of doing this. two copies and a level 2 BLAS or our own loop
  // working direcly on the input/output memory -E
  for (int32_t i = 0; i < gc.nelx; i++)
    for (int32_t k = 0; k < gc.nelz; k++)
      for (int32_t j = 0; j < gc.nely; j++) {

        getEdof(edof, i, j, k, ny, nz);

        const uint_fast32_t elementIndex =
            i * gc.nely * gc.nelz + k * gc.nely + j;
        const MATRIXPRECISION elementScale =
            gc.Emin + pow(x[elementIndex], gc.penal) * (gc.E0 - gc.Emin);

        for (int ii = 0; ii < 24; ii++)
          diag[edof[ii]] += elementScale * ke[ii][ii];
      }

  // apply boundaryConditions
  struct FixedDofs fd = getFixedDof(gc.nelx, gc.nely, gc.nelz);
  for (int i = 0; i < fd.n; i++)
    diag[fd.idx[i]] = 1.0;
  free(fd.idx);
}

/**
 * @brief Performs the calculations for c(x) (eq. (1).1) and dc/dx_e (eq (4)) for every element in the grid
 * 
 * @param gc In: The grid context of the current problem
 * @param x In: The current design variables
 * @param u In: The global displacement vector
 * @param c Output: The total change in design variables over all elements
 * @param dcdx Output: The value of dc/dx_e for all elements in the grid
 */
void getComplianceAndSensetivity(const struct gridContext gc , DTYPE *x,
                                 MATRIXPRECISION *u, DTYPE *c, DTYPE *dcdx) {

  uint_fast32_t edof[24];
  MATRIXPRECISION ke[24][24];
  MATRIXPRECISION u_local[24]; //corresponds to u_e
  MATRIXPRECISION tmp[24];
  getKE(ke, gc.nu, gc.elementSizeX, gc.elementSizeY, gc.elementSizeZ); //get k_e aka. k_0, which is constant

  const int ny = gc.nely + 1;
  const int nz = gc.nelz + 1; //?Why only in y and z direction?

  (*c) = 0.0;
  MATRIXPRECISION clocal;

  //Loop over entire grid
  for (int32_t i = 0; i < gc.nelx; i++)
    for (int32_t k = 0; k < gc.nelz; k++)
      for (int32_t j = 0; j < gc.nely; j++) {

        getEdof(edof, i, j, k, ny, nz);
        const uint_fast32_t elementIndex =
            i * gc.nely * gc.nelz + k * gc.nely + j; //elements are accessed first on z-axis, then y-axis and finally x-axis
				//eg (0,1,2,3) are the nodes associated with elements (0,0,0), (0,1,0), (0,2,0) (3 elements = 4 nodes)
				//When we get j=0, k=1, elementIndex=4, and we will now access (0,0,1), (0,1,1), (0,2,1) and so forth

        // copy to local buffer for blas use
        for (int ii = 0; ii < 24; ii++)
          u_local[ii] = u[edof[ii]];

        // clocal = ulocal^T * ke * ulocal
		  //This line calculates ke(24x24 matrix) * ulocal (24x1 vector)
        cblas_dsymv(CblasRowMajor, CblasUpper, 24, 1.0, (MATRIXPRECISION *)ke,
                    24, u_local, 1, 0.0, tmp, 1);
        clocal = 0.0;
		  //This line calculates ue^T *(ke*ulocal)
		  //ue^T is a [1x24] row vector and (ke*ulocal) is a [24x1] col. vector. Result is a single number
        for (int ii = 0; ii < 24; ii++)
          clocal += u_local[ii] * tmp[ii];

        // apply contribution to c and dcdx
		  //This value is c(x)=sum(xe^p)*ue^T*ke*ue
        (*c) += clocal *
                (gc.Emin + pow(x[elementIndex], gc.penal) * (gc.E0 - gc.Emin));
			
			//This value is eq. (4), the inital estimate of sensitivity
        dcdx[elementIndex] = clocal * (-gc.penal * (gc.E0 - gc.Emin) *
                                       pow(x[elementIndex], gc.penal - 1));
      }
}
/**
 * @brief 
 * 
 * @param gc The grid context of the current problem
 * @param x The current element densities
 * @param nswp Number of iterations to run the preconditioner
 * @param omega Damping factor for the preconditioner
 * @param invD The inverted matrix diagonal of Ke
 * @param u In: Current estimate to be preconditioned. Out: Result of preconditiong
 * @param b Right-hand side of preconditiong problem
 * @param tmp output: Temporary work buffer of size u,b
 * 
 */
void preconditionDampedJacobi(const struct gridContext gc /* in */, DTYPE *x /* in: denseties */, const uint_fast32_t nswp /* in: number of iterations to run */,
                const MATRIXPRECISION omega /* in: damping factor */, const MATRIXPRECISION *invD /* in: inverted matrix diagonal */,
                MATRIXPRECISION *u /* in/out: current solution to be preconditioned */, const MATRIXPRECISION *b /* in: right hand side vector*/,
                MATRIXPRECISION *tmp /* out: temporary work buffer of size u,b */) {

  const uint_fast32_t ndof = 3 * (gc.nelx + 1) * (gc.nely + 1) * (gc.nelz + 1);

  for (int s = 0; s < nswp; s++) {
    applyStateOperator(gc, x, u, tmp);

    for (int i = 0; i < ndof; i++)
      u[i] += omega * invD[i] * (b[i] - tmp[i]);
  }
}

MATRIXPRECISION norm(MATRIXPRECISION *v,
                     const uint_fast32_t size /* length of v */) {
  MATRIXPRECISION val = 0.0;
  for (uint_fast32_t i = 0; i < size; i++)
    val += v[i] * v[i];
  return sqrt(val);
}

MATRIXPRECISION innerProduct(MATRIXPRECISION *a, MATRIXPRECISION *b,
                             const uint_fast32_t size /* length of a and b */) {
  MATRIXPRECISION val = 0.0;
  for (uint_fast32_t i = 0; i < size; i++)
    val += a[i] * b[i];
  return val;
}

/**
 * @brief Approximates the solution u to Ku=b with tolerance tol. Relies on stencils for applying the K matrix.
 * In other words, performs one iteration of the FE method
 * Uses some fancy method we need to be made aware of
 * 
 * @param gc The current gridContext with global values
 * @param x The current element densities to be updated
 * @param nswp Number of jacobian sweeps
 * @param tol Tolerance required for the solve
 * @param finalIter output: The number of iterations used
 * @param finalRes output: The final residual after iterating
 * @param b The forcing vector applied to the system
 * @param u in: The initial guess / out: Final solution to global displacement?
 */
void solveStateCG(
    const struct gridContext gc /* in */, DTYPE *x /* in: element denseties */,
    const int nswp /* in: number of jacobian sweeps */,
    const MATRIXPRECISION tol /* in: required solve tolerance */,
    int *finalIter /* out: used iterations */,
    float *finalRes /* out: final residual */,
    MATRIXPRECISION *b /* in: forcing vector */,
    MATRIXPRECISION *u /* in/out: initial guess / final solution */) {

  const uint_fast32_t ndof = 3 * (gc.nely + 1) * (gc.nelx + 1) * (gc.nelz + 1);

  // allocate needed memory
  CGVECTORPRECISION *r = malloc(sizeof(CGVECTORPRECISION) * ndof); //Residual of b-A*x
  CGVECTORPRECISION *z = malloc(sizeof(CGVECTORPRECISION) * ndof); //Preconditioned residual
  CGVECTORPRECISION *p = malloc(sizeof(CGVECTORPRECISION) * ndof); 
  CGVECTORPRECISION *q = malloc(sizeof(CGVECTORPRECISION) * ndof);
  MATRIXPRECISION *invD = malloc(sizeof(MATRIXPRECISION) * ndof);
  MATRIXPRECISION *tmp = malloc(sizeof(MATRIXPRECISION) * ndof);

  // note that for larger systems it might increase convergence rate to set initial guess to zero
  // for (uint_fast32_t i = 0; i < ndof; i++)
  //   u[i] = 0.0;

  // setup residual vector
  applyStateOperator(gc, x, u, r); //Computes r=A*x
  for (uint_fast32_t i = 0; i < ndof; i++)
    r[i] = b[i] - r[i]; //Residual r=b-A*x

  // setup inverse diagonal of system matrix
  generateMatrixDiagonal(gc, x, invD);
  for (uint_fast32_t i = 0; i < ndof; i++)
    invD[i] = 1.0 / invD[i];

  // setup scalars
  const MATRIXPRECISION omega = 0.6;
  const CGVECTORPRECISION bnorm = norm(b, ndof);
  const int maxIter = 10000;
  CGVECTORPRECISION rho;
  CGVECTORPRECISION rhoold = 0.0;
  CGVECTORPRECISION dpr;
  CGVECTORPRECISION alpha;

  // begin cg loop
  for (int iter = 0; iter < maxIter; iter++) {

    // get preconditioned vector
    // FIXME use VCYCLE multigrid
    for (uint_fast32_t i = 0; i < ndof; i++)
      z[i] = 0.0;
    preconditionDampedJacobi(gc, x, nswp, omega, invD, z, r, tmp);
    CGVECTORPRECISION
    rho = innerProduct(r, z, ndof);

    if (iter == 0)
      for (uint_fast32_t i = 0; i < ndof; i++)
        p[i] = z[i];
    else {
      CGVECTORPRECISION beta = rho / rhoold;
      for (uint_fast32_t i = 0; i < ndof; i++)
        p[i] = beta * p[i] + z[i];
    }

    applyStateOperator(gc, x, p, q);
    dpr = innerProduct(p, q, ndof);
    alpha = rho / dpr;
    rhoold = rho;

    for (uint_fast32_t i = 0; i < ndof; i++)
      u[i] += alpha * p[i];

    for (uint_fast32_t i = 0; i < ndof; i++)
      r[i] -= alpha * q[i];

    CGVECTORPRECISION relres = norm(r, ndof) / bnorm;

    (*finalIter) = iter;
    (*finalRes) = relres;

    if (relres < tol)
      break;
  }

  // free needed memory
  free(r);
  free(invD);
  free(z);
  free(p);
  free(q);
  free(tmp);
}

/**
 * @brief Main function to perform FE analysis in 3d on a coarse grid
 * 
 * @param nelx Number of elements in the x-direction of the grid
 * @param nely Number of elements in the y-direction of the grid
 * @param nelz Number of element in the z-direction of the grid
 * @param volfrac The wanted volume fraction level
 * @param penal Penalty factor for the power-law approach
 * @param rmin Minimum radius around the element at which the weighting factor is calculated
 * @param cgtol Coarse grid tolerance?
 * @param cgmax Coarse grid max?
 * @param xPhys Space set aside for doubles representing grid
 */
void top3dcg(const uint_fast32_t nelx, const uint_fast32_t nely,
             const uint_fast32_t nelz, const float volfrac, const float penal,
             const float rmin, const float cgtol, const uint_fast32_t cgmax,
             DTYPE *xPhys) {

  struct gridContext gridContext;
  gridContext.E0 = 1;
  gridContext.Emin = 1e-6;
  gridContext.nu = 0.3;
  gridContext.penal = penal;
  gridContext.nelx = nelx;
  gridContext.nely = nely;
  gridContext.nelz = nelz;

  gridContext.elementSizeX = 0.5;
  gridContext.elementSizeY = 0.5;
  gridContext.elementSizeZ = 0.5;

  const uint_fast32_t nelem = nelx * nely * nelz; //Number of elements total in the grid, nelx*nely*nelz

  const uint_fast32_t nx = nelx + 1;
  const uint_fast32_t ny = nely + 1;
  const uint_fast32_t nz = nelz + 1;

  const uint_fast32_t ndof = 3 * nx * ny * nz; //Number of degrees of freedom. This value because each node has x,y,z movement

  //Both matrices have this size as displacement/force can be in the x,y,z-direction for all nx*ny*nz nodes
  MATRIXPRECISION *const F = malloc(sizeof(MATRIXPRECISION) * ndof);
  MATRIXPRECISION *const U = malloc(sizeof(MATRIXPRECISION) * ndof);

  for (uint_fast32_t i = 0; i < ndof; i++) {
    F[i] = 0.0;
    U[i] = 0.0;
  }

  //? Why this block? 
  //! Smth. about setting up the force vector for this problem?
  for (int j = 0; j < ny; j++) {
    const int i = nx - 1;
    const int k = 0;
    const uint_fast32_t nidx = i * ny * nz + k * ny + j;
    F[3 * nidx + 2] = -1.0;
  }

  DTYPE *dc = malloc(sizeof(DTYPE) * nelem); //Change in c-value
  DTYPE *dv = malloc(sizeof(DTYPE) * nelem); //Volumetric change
  DTYPE *x = malloc(sizeof(DTYPE) * nelem); //Design variables
  DTYPE *xnew = malloc(sizeof(DTYPE) * nelem); //New design variables
  DTYPE c = 0.0;
  //Populate design space with initial volume shares
  for (uint_fast64_t i = 0; i < nelem; i++) {
    x[i] = volfrac;
    xPhys[i] = volfrac;
    dv[i] = 1.0;
  }

  applyDensityFilterGradient(gridContext, rmin, dv);

  unsigned int loop = 0;
  float change = 1;

  while ((change > 1e-2) && (loop < 100)) {

    loop++;

    int cgiter;
    float cgres;
    solveStateCG(gridContext, xPhys, 2, cgtol, &cgiter, &cgres, F, U); //FE-step, solve matrix equation KU=F for matrix U

    getComplianceAndSensetivity(gridContext, xPhys, U, &c, dc);
    applyDensityFilterGradient(gridContext, rmin, dc);

    DTYPE g = 0.0;
    DTYPE vol = 0.0;
    for (uint_least32_t i = 0; i < nelem; i++) {
      g += xPhys[i];
      vol += xPhys[i];
    }
    g = g / (DTYPE)nelem - volfrac; //Current error in volume-per-element value, I think
    vol /= (DTYPE)nelem; //Current volume-per-element value, I think

    // update denseties
	 //! This part corresponds to optimality criteria update
	 //! where we find the value of lambda for our Lagrangian
    DTYPE l1 = 0.0, l2 = 1e9, move = 0.2;
    while ((l2 - l1) / (l1 + l2) > 1e-6) {
      DTYPE lmid = 0.5 * (l2 + l1);
      DTYPE gt = 0.0;
      for (uint_least32_t i = 0; i < nelem; i++) {
        xnew[i] =
            MAX(0.0, MAX(x[i] - move,
                         MIN(1.0, MIN(x[i] + move,
                                      x[i] * sqrt(-dc[i] / (dv[i] * lmid))))));
        gt += dv[i] * (xnew[i] - x[i]);
      }
      gt += g;
      if (gt > 0)
        l1 = lmid;
      else
        l2 = lmid;
    }

    // compute amount of change
    change = 0.0;
    for (uint_least32_t i = 0; i < nelem; i++) {
      change = MAX(change, fabs(x[i] - xnew[i]));
      x[i] = xnew[i];
    }

    applyDensityFilter(gridContext, rmin, x, xPhys);

    printf(
        "It.:%4i Obj.:%6.3e Vol.:%6.3f ch.:%4.2f relres: %4.2e iters: %4i \n",
        loop, c, vol, change, cgres, cgiter);
  }

  free(F);
  free(U);
  free(dc);
  free(dv);
  free(x);
  free(xnew);
}

void main(void) {

  const uint_fast32_t nelx = 30;
  const uint_fast32_t nely = 15;
  const uint_fast32_t nelz = 15;
  DTYPE *xPhys = malloc(sizeof(DTYPE) * nelx * nely * nelz); //Memory set aside for nelx * nely * nelz doubles

  top3dcg(nelx, nely, nelz, 0.2, 3, 1.5, 1e-5, 1000, xPhys);
  writeDensityVtkFile(nelx, nely, nelz, xPhys, "output.vtu");

  free(xPhys);
}
