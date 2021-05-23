#include "dist/include/openssl/evp.h"
#include "dist/include/openssl/bio.h"
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

typedef int (*port_read_t)(const char*, int);


char* SUPPORTED_CHFS[20] = {"SHA256", "SHA3-512", "SM3", "BLAKE2S-256", "SHA3-256", "SHA3-224",
                            "SHA2-384", "SHA2-512", "SHA512-224", "SHAKE-256", "SHA-1", "SHA3-384",
                            "SHAKE-128", "MD5-SHA1", "MD5", "SHA2-224", "BLAKE2B-512",
                            "SHA2-512/256", "KECCAK-KMAC-128", "KECCAK-KMAC-256" };

char** get_supported_chfs() {
  return SUPPORTED_CHFS;
}

unsigned char digest(const char* mdname, port_read_t fn) {
  EVP_MD_CTX *ctx = NULL;
  EVP_MD *md = NULL;
  unsigned int len = 0;
  unsigned char *outdigest = NULL;
  unsigned char errnum = 0;
  int available;
  unsigned int buffer_size = 1024;
  const char buf[buffer_size];

  ctx = EVP_MD_CTX_new();
  if (ctx == NULL)
    goto err;

  // TODO: Have opinion on provider and criteria.
  md = EVP_MD_fetch(NULL, mdname, NULL);
  if (md == NULL) {
    errnum = 1;
    goto err;
  }

  if (!EVP_DigestInit_ex(ctx, md, NULL)) {
    errnum = 2;
    goto err;
  }

  while (1) {
    available = fn(buf, buffer_size);
    if (available == -1) break;
    if (available > 0) {
      if (!EVP_DigestUpdate(ctx, buf, available)) {
        errnum = 3;
        goto err;
      }
    }
  }

  outdigest = OPENSSL_malloc(EVP_MD_size(md));
  if (outdigest == NULL) {
    errnum = 4;
    goto err;
  }

  if (!EVP_DigestFinal_ex(ctx, outdigest, &len)) {
    errnum = 5;
    goto err;
  }

  write(1, outdigest, len);
 err:
  OPENSSL_free(outdigest);
  EVP_MD_free(md);
  EVP_MD_CTX_free(ctx);

  return errnum;
}

