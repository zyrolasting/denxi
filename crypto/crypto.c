#include "dist/include/openssl/evp.h"
#include "dist/include/openssl/bio.h"
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

typedef unsigned char* (*port_read_t)(int*);

#define SUPPORTED_CHF_COUNT 10
#define BUFFER_SIZE 1024

char* XIDEN_SUPPORTED_CHFS[SUPPORTED_CHF_COUNT] = {
  "MD5",
  "SHA1",
  "SHA2-224",
  "SHA2-384",
  "SHA2-512",
  "SHA256",
  "SHA3-224",
  "SHA3-256",
  "SHA3-384",
  "SHA3-512"
};

const unsigned int XIDEN_AVAILABLE_CHF_COUNT = SUPPORTED_CHF_COUNT;
const unsigned int XIDEN_DEFAULT_CHF_INDEX = 8; // SHA3-384

EVP_MD* get_chf_handle(unsigned int mdindex) {
  return (mdindex < XIDEN_AVAILABLE_CHF_COUNT)
    ? EVP_MD_fetch(NULL, XIDEN_SUPPORTED_CHFS[mdindex], NULL)
    : NULL;
}

int get_digest_size(const EVP_MD* md) {
  return EVP_MD_size(md);
}

unsigned char make_digest_unsafe(EVP_MD* md, char* outdigest, port_read_t fn) {
  unsigned int len = 0;
  char errnum = 0;
  char* buf;

  EVP_MD_CTX* ctx = EVP_MD_CTX_new();
  if (ctx == NULL) {
    errnum = 1;
    goto err;
  }

  if (!EVP_DigestInit_ex(ctx, md, NULL)) {
    errnum = 2;
    goto err;
  }

  while ((buf = fn(&len)) != NULL) {
    if (EVP_DigestUpdate(ctx, buf, len) == 0) {
      errnum = 3;
      goto err;
    }
  }

  if (!EVP_DigestFinal_ex(ctx, outdigest, NULL)) {
    errnum = 4;
  }

 err:
  EVP_MD_free(md);
  EVP_MD_CTX_free(ctx);

  return errnum;
}
