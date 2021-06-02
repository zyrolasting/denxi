/*
Define simplified interface for libcrypto.3, specifically message
digest creation, signature creation using asymmetric keys, and
signature verification. Ensure availability for a fixed set of
cryptographic hash functions.

Documentation is at the bottom of the file in a large block comment.
Other comments may include section ids (e.g. [some-section]). Search
this file for those ids to read related documentation for the
commented code.
*/

#include "dist/include/openssl/evp.h"
#include "dist/include/openssl/bio.h"
#include "dist/include/openssl/pem.h"
#include "dist/include/openssl/err.h"
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

#define SUPPORTED_CHF_COUNT 10
#define BUFFER_SIZE 1024

typedef unsigned char byte;
typedef unsigned int uint;
typedef byte* (*port_read_t)(int*, uint);

const uint XIDEN_AVAILABLE_CHF_COUNT = SUPPORTED_CHF_COUNT;
const uint XIDEN_DEFAULT_CHF_INDEX = 8; // SHA3-384

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


EVP_MD* xiden_load_chf(uint chf_index) {
  return (chf_index < XIDEN_AVAILABLE_CHF_COUNT)
    ? EVP_MD_fetch(NULL, XIDEN_SUPPORTED_CHFS[chf_index], NULL)
    : NULL;
}


int xiden_get_digest_size(EVP_MD* p_md) {
  return EVP_MD_size(p_md);
}


int xiden_make_digest(EVP_MD* p_md, byte* p_digest, port_read_t read_more) {
  if (p_md == NULL || p_digest == NULL || read_more == NULL)
    goto end;

  EVP_MD_CTX* p_md_ctx = EVP_MD_CTX_new();
  if (p_md_ctx == NULL) goto end;

  if (EVP_DigestInit_ex(p_md_ctx, p_md, NULL) == 0) goto end;
  
  uint read_buffer_length = 0;
  char* read_buffer = NULL;
  uint calls = 0;

  while ((read_buffer = read_more(&read_buffer_length, calls)) != NULL) {
    ++calls;
    if (EVP_DigestUpdate(p_md_ctx, read_buffer, read_buffer_length) != 1) goto end;
  }
  
  return EVP_DigestFinal_ex(p_md_ctx, p_digest, NULL);
 end:
  if (p_md) EVP_MD_free(p_md);
  if (p_md_ctx) EVP_MD_CTX_free(p_md_ctx);
}

int xiden_start_signature(EVP_MD* p_md,
                          EVP_MD_CTX* p_ctx,
                          char* p_private_key_content,
                          char* p_private_key_password,
                          char* p_digest,
                          size_t digest_length) {
  BIO* p_mem = BIO_new_mem_buf(p_private_key_content, -1);
  if (p_mem == NULL) goto end;

  EVP_PKEY* p_private_key = PEM_read_bio_PrivateKey(p_mem, NULL, NULL, (void*)p_private_key_password);
  if (p_private_key == NULL) goto end;

  if (EVP_DigestSignInit(p_ctx, NULL, p_md, NULL, p_private_key) != 1) goto end;

  return EVP_DigestSignUpdate(p_ctx, p_digest, digest_length);  
 end:
  BIO_free(p_mem);

  // Caller must continue to xiden_find_signature_size
}


// Note that the operation ordinal starts at non-zero here. The Racket
// runtime sees the use of these functions as part of a longer signing
// operation.
size_t xiden_find_signature_size(EVP_MD_CTX* p_ctx) {  
  size_t siglen = 0;
  
  EVP_DigestSignFinal(p_ctx, NULL, &siglen);

  return siglen;
  // Caller must continue to xiden_end_signature
}


int xiden_end_signature(EVP_MD_CTX* p_ctx, char* p_signature, size_t signature_length) {
  if (p_ctx == NULL || p_signature == NULL || signature_length == 0) goto end;  
  return EVP_DigestSignFinal(p_ctx, p_signature, &signature_length);
 end:
  if (p_ctx) EVP_MD_CTX_destroy(p_ctx);
  // Signature creation complete
}


int xiden_verify_signature(EVP_MD* p_md,
                           char* p_signature,
                           size_t signature_length,
                           char* p_public_key_content,
                           char* p_digest,
                           size_t digest_length) {
  BIO* p_mem = BIO_new_mem_buf(p_public_key_content, -1);
  if (p_mem == NULL) goto end;

  EVP_PKEY* p_key = PEM_read_bio_PUBKEY(p_mem, NULL, NULL, NULL);
  if (p_key == NULL) goto end;
  
  EVP_MD_CTX* p_ctx = EVP_MD_CTX_new();
  if (p_ctx == NULL) goto end;
  
  if (EVP_DigestVerifyInit(p_ctx, NULL, p_md, NULL, p_key) != 1) goto end;
  if (EVP_DigestVerifyUpdate(p_ctx, p_digest, digest_length) != 1) goto end;
  
  return EVP_DigestVerifyFinal(p_ctx, p_signature, signature_length);
 end:
  if (p_mem) BIO_free(p_mem);
  if (p_ctx) EVP_MD_CTX_destroy(p_ctx);
}

#ifdef XIDEN_CRYPTO_MAIN
byte* read_dummy(int*, uint);

int main() {
  for (int chf_index = 0; chf_index < XIDEN_AVAILABLE_CHF_COUNT; ++chf_index) {
    printf("%d: %s\n",
           chf_index,
           XIDEN_SUPPORTED_CHFS[chf_index]);

    EVP_MD* p_md = xiden_load_chf(chf_index);
    if (p_md == NULL) {
      printf("Could not fetch EVP_MD! Skipping.\n");
      continue;
    }
    
    int digest_size = xiden_get_digest_size(p_md);
    printf("Digest size: %d\n", digest_size);

    if (digest_size <= 0) {
      printf("Nonsensical size. Skipping.\n");
      continue;
    }

    byte* p_digest = (byte*)malloc(digest_size);

    xiden_make_digest(p_md, p_digest, read_dummy);
    for (int i = 0; i < digest_size; ++i) {
      printf("%x", p_digest[i]);
    }
    free(p_digest);

    printf("\n\n");
  }
}

byte* read_dummy(int* p_read, uint calls) {
  if (calls == 0) {
    (*p_read) = 5;
    return "abcde";
  } else return NULL;
}
#endif
