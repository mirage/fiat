#ifdef OCAML_FIAT_USE_INT128
#include "p256_64.h"
#define NLIMBS 4
typedef uint64_t limb_t;
#else
#define NLIMBS 8
#include "p256_32.h"
typedef uint32_t limb_t;
#endif

typedef limb_t fe[NLIMBS];

#include <caml/bigarray.h>
#include <caml/memory.h>

#define fe_add fiat_p256_add
#define fe_sub fiat_p256_sub
#define fe_opp fiat_p256_opp

#define fe_mul fiat_p256_mul
#define fe_sqr fiat_p256_square

#define fe_tobytes fiat_p256_to_bytes
#define fe_frombytes fiat_p256_from_bytes

static limb_t fe_nz(const limb_t in1[NLIMBS]) {
  limb_t ret;
  fiat_p256_nonzero(&ret, in1);
  return ret;
}

static void fe_copy(limb_t out[NLIMBS], const limb_t in1[NLIMBS]) {
  for (int i = 0; i < NLIMBS; i++) {
    out[i] = in1[i];
  }
}

static void fe_cmovznz(limb_t out[NLIMBS], limb_t t, const limb_t z[NLIMBS],
                       const limb_t nz[NLIMBS]) {
  fiat_p256_selectznz(out, !!t, z, nz);
}

static void fe_from_montgomery(fe x) {
  fiat_p256_from_montgomery(x, x);
}

// fe_inv calculates |out| = |in|^{-1}
//
// Based on Fermat's Little Theorem:
//   a^p = a (mod p)
//   a^{p-1} = 1 (mod p)
//   a^{p-2} = a^{-1} (mod p)
static void fe_inv(fe out, const fe in) {
  fe ftmp, ftmp2;
  // each e_I will hold |in|^{2^I - 1}
  fe e2, e4, e8, e16, e32, e64;

  fe_sqr(ftmp, in);  // 2^1
  fe_mul(ftmp, in, ftmp);  // 2^2 - 2^0
  fe_copy(e2, ftmp);
  fe_sqr(ftmp, ftmp);  // 2^3 - 2^1
  fe_sqr(ftmp, ftmp);  // 2^4 - 2^2
  fe_mul(ftmp, ftmp, e2);  // 2^4 - 2^0
  fe_copy(e4, ftmp);
  fe_sqr(ftmp, ftmp);  // 2^5 - 2^1
  fe_sqr(ftmp, ftmp);  // 2^6 - 2^2
  fe_sqr(ftmp, ftmp);  // 2^7 - 2^3
  fe_sqr(ftmp, ftmp);  // 2^8 - 2^4
  fe_mul(ftmp, ftmp, e4);  // 2^8 - 2^0
  fe_copy(e8, ftmp);
  for (size_t i = 0; i < 8; i++) {
    fe_sqr(ftmp, ftmp);
  }  // 2^16 - 2^8
  fe_mul(ftmp, ftmp, e8);  // 2^16 - 2^0
  fe_copy(e16, ftmp);
  for (size_t i = 0; i < 16; i++) {
    fe_sqr(ftmp, ftmp);
  }  // 2^32 - 2^16
  fe_mul(ftmp, ftmp, e16);  // 2^32 - 2^0
  fe_copy(e32, ftmp);
  for (size_t i = 0; i < 32; i++) {
    fe_sqr(ftmp, ftmp);
  }  // 2^64 - 2^32
  fe_copy(e64, ftmp);
  fe_mul(ftmp, ftmp, in);  // 2^64 - 2^32 + 2^0
  for (size_t i = 0; i < 192; i++) {
    fe_sqr(ftmp, ftmp);
  }  // 2^256 - 2^224 + 2^192

  fe_mul(ftmp2, e64, e32);  // 2^64 - 2^0
  for (size_t i = 0; i < 16; i++) {
    fe_sqr(ftmp2, ftmp2);
  }  // 2^80 - 2^16
  fe_mul(ftmp2, ftmp2, e16);  // 2^80 - 2^0
  for (size_t i = 0; i < 8; i++) {
    fe_sqr(ftmp2, ftmp2);
  }  // 2^88 - 2^8
  fe_mul(ftmp2, ftmp2, e8);  // 2^88 - 2^0
  for (size_t i = 0; i < 4; i++) {
    fe_sqr(ftmp2, ftmp2);
  }  // 2^92 - 2^4
  fe_mul(ftmp2, ftmp2, e4);  // 2^92 - 2^0
  fe_sqr(ftmp2, ftmp2);  // 2^93 - 2^1
  fe_sqr(ftmp2, ftmp2);  // 2^94 - 2^2
  fe_mul(ftmp2, ftmp2, e2);  // 2^94 - 2^0
  fe_sqr(ftmp2, ftmp2);  // 2^95 - 2^1
  fe_sqr(ftmp2, ftmp2);  // 2^96 - 2^2
  fe_mul(ftmp2, ftmp2, in);  // 2^96 - 3

  fe_mul(out, ftmp2, ftmp);  // 2^256 - 2^224 + 2^192 + 2^96 - 3
}

// Group operations
// ----------------
//
// Building on top of the field operations we have the operations on the
// elliptic curve group itself. Points on the curve are represented in Jacobian
// coordinates.
//
// Both operations were transcribed to Coq and proven to correspond to naive
// implementations using Affine coordinates, for all suitable fields.  In the
// Coq proofs, issues of constant-time execution and memory layout (aliasing)
// conventions were not considered. Specification of affine coordinates:
// <https://github.com/mit-plv/fiat-crypto/blob/79f8b5f39ed609339f0233098dee1a3c4e6b3080/src/Spec/WeierstrassCurve.v#L28>
// As a sanity check, a proof that these points form a commutative group:
// <https://github.com/mit-plv/fiat-crypto/blob/79f8b5f39ed609339f0233098dee1a3c4e6b3080/src/Curves/Weierstrass/AffineProofs.v#L33>

// point_double calculates 2*(x_in, y_in, z_in)
//
// The method is taken from:
//   http://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-3.html#doubling-dbl-2001-b
//
// Coq transcription and correctness proof:
// <https://github.com/mit-plv/fiat-crypto/blob/79f8b5f39ed609339f0233098dee1a3c4e6b3080/src/Curves/Weierstrass/Jacobian.v#L93>
// <https://github.com/mit-plv/fiat-crypto/blob/79f8b5f39ed609339f0233098dee1a3c4e6b3080/src/Curves/Weierstrass/Jacobian.v#L201>
//
// Outputs can equal corresponding inputs, i.e., x_out == x_in is allowed.
// while x_out == y_in is not (maybe this works, but it's not tested).
static void point_double(fe x_out, fe y_out, fe z_out,
                         const fe x_in, const fe y_in, const fe z_in) {
  fe delta, gamma, beta, ftmp, ftmp2, tmptmp, alpha, fourbeta;
  // delta = z^2
  fe_sqr(delta, z_in);
  // gamma = y^2
  fe_sqr(gamma, y_in);
  // beta = x*gamma
  fe_mul(beta, x_in, gamma);

  // alpha = 3*(x-delta)*(x+delta)
  fe_sub(ftmp, x_in, delta);
  fe_add(ftmp2, x_in, delta);

  fe_add(tmptmp, ftmp2, ftmp2);
  fe_add(ftmp2, ftmp2, tmptmp);
  fe_mul(alpha, ftmp, ftmp2);

  // x' = alpha^2 - 8*beta
  fe_sqr(x_out, alpha);
  fe_add(fourbeta, beta, beta);
  fe_add(fourbeta, fourbeta, fourbeta);
  fe_add(tmptmp, fourbeta, fourbeta);
  fe_sub(x_out, x_out, tmptmp);

  // z' = (y + z)^2 - gamma - delta
  fe_add(delta, gamma, delta);
  fe_add(ftmp, y_in, z_in);
  fe_sqr(z_out, ftmp);
  fe_sub(z_out, z_out, delta);

  // y' = alpha*(4*beta - x') - 8*gamma^2
  fe_sub(y_out, fourbeta, x_out);
  fe_add(gamma, gamma, gamma);
  fe_sqr(gamma, gamma);
  fe_mul(y_out, alpha, y_out);
  fe_add(gamma, gamma, gamma);
  fe_sub(y_out, y_out, gamma);
}

CAMLprim value fiat_p256_caml_mul(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_p256_mul(Caml_ba_data_val(out), Caml_ba_data_val(a), Caml_ba_data_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value fiat_p256_caml_add(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_p256_add(Caml_ba_data_val(out), Caml_ba_data_val(a), Caml_ba_data_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value fiat_p256_caml_sub(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_p256_sub(Caml_ba_data_val(out), Caml_ba_data_val(a), Caml_ba_data_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value fiat_p256_caml_from_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p256_from_bytes(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value fiat_p256_caml_to_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p256_to_bytes(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value fiat_p256_caml_sqr(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p256_square(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value fiat_p256_caml_from_montgomery(value x)
{
	CAMLparam1(x);
	limb_t *l = Caml_ba_data_val(x);
	fiat_p256_from_montgomery(l, l);
	CAMLreturn(Val_unit);
}


CAMLprim value fiat_p256_caml_nz(value x)
{
	CAMLparam1(x);
	CAMLreturn(Val_bool(fe_nz(Caml_ba_data_val(x))));
}

CAMLprim value fiat_p256_caml_cmovznz(value out, value cond, value z, value nz)
{
	CAMLparam4(out, cond, z, nz);
	fiat_p256_selectznz(
		Caml_ba_data_val(out),
		Bool_val(cond),
		Caml_ba_data_val(z),
		Caml_ba_data_val(nz)
	);
	CAMLreturn(Val_unit);
}

CAMLprim value fiat_p256_caml_inv(value out, value in)
{
	CAMLparam2(out, in);
	fe_inv(Caml_ba_data_val(out), Caml_ba_data_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value fiat_p256_caml_point_double(value out, value in)
{
	CAMLparam2(out, in);
	point_double(
		Caml_ba_data_val(Field(out, 0)),
		Caml_ba_data_val(Field(out, 1)),
		Caml_ba_data_val(Field(out, 2)),
		Caml_ba_data_val(Field(in, 0)),
		Caml_ba_data_val(Field(in, 1)),
		Caml_ba_data_val(Field(in, 2))
	);
	CAMLreturn(Val_unit);
}
