#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <mpfr.h>

int stream_out_mpq(
  mpq_t      quant,
  size_t     precision,
  int        zeros_prec /* = -1 */,
  mpfr_rnd_t rnd /* = GMP_RNDN */,
  char **    bufPtr)
{
  static const size_t extend_by_digits = 6U;

  mpfr_t tempfb;
  mpfr_t tempfnum;
  mpfr_t tempfden;

  mpfr_init(tempfb);
  mpfr_init(tempfnum);
  mpfr_init(tempfden);

  // Convert the rational number to a floating-point, extending the
  // floating-point to a large enough size to get a precise answer.

  mp_prec_t num_prec =
    (mpfr_prec_t)mpz_sizeinbase(mpq_numref(quant), 2);
  num_prec += extend_by_digits*64;
  if (num_prec < MPFR_PREC_MIN)
    num_prec = MPFR_PREC_MIN;

  mpfr_set_prec(tempfnum, num_prec);
  mpfr_set_z(tempfnum, mpq_numref(quant), rnd);

  mp_prec_t den_prec =
    (mpfr_prec_t)mpz_sizeinbase(mpq_denref(quant), 2);
  den_prec += extend_by_digits*64;
  if (den_prec < MPFR_PREC_MIN)
    den_prec = MPFR_PREC_MIN;

  mpfr_set_prec(tempfden, den_prec);
  mpfr_set_z(tempfden, mpq_denref(quant), rnd);

  mpfr_set_prec(tempfb, num_prec + den_prec);
  mpfr_div(tempfb, tempfnum, tempfden, rnd);

  if (mpfr_asprintf(bufPtr, "%.*RNf", precision, tempfb) < 0) {
    mpfr_clear(tempfb);
    mpfr_clear(tempfnum);
    mpfr_clear(tempfden);
    return -1;
  }

  char * buf = *bufPtr;

  if (zeros_prec >= 0) {
    size_t index = strlen(buf);
    size_t point = 0;
    for (size_t i = 0; i < index; i++) {
      if (buf[i] == '.') {
        point = i;
        break;
      }
    }
    if (point > 0) {
      while (--index >= (point + 1 + (size_t)zeros_prec) &&
             buf[index] == '0')
        buf[index] = '\0';
      if (index >= (point + (size_t)zeros_prec) &&
          buf[index] == '.')
        buf[index] = '\0';
    }
  }

  mpfr_clear(tempfb);
  mpfr_clear(tempfnum);
  mpfr_clear(tempfden);

  return 0;
}

void rational_to_str(
  signed long num,
  unsigned long den,
  unsigned int rnd_mode,
  size_t prec,
  char ** bufPtr)
{
  mpq_t val;
  mpq_init(val);
  mpq_set_si(val, num, den);
  stream_out_mpq(val, prec, -1, rnd_mode, bufPtr);
  mpq_clear(val);
}
