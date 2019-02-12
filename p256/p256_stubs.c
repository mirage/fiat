#ifdef OCAML_FIAT_USE_INT128
#include "p256_64.h"
typedef uint64_t limb_t;
#else
#include "p256_32.h"
typedef uint32_t limb_t;
#endif

#include <caml/bigarray.h>
#include <caml/memory.h>

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
	limb_t ret;
	fiat_p256_nonzero(&ret, Caml_ba_data_val(x));
	CAMLreturn(Val_bool(ret));
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
