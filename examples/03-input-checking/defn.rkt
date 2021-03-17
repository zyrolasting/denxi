#lang xiden

(output "default"
        inp := (input-ref "data")
        (resolve-input inp))

(input "data"
       (sources (text-source "code code code\n"))
       (integrity 'sha384 (base64 "qAhYrIsnCp+iSA1sPg74sfLbv/PsRSUVL0K6krxwRAwvdOEVxloL089YFXw1xukS"))
       (signature "https://sagegerard.com/public.pem"
                  (base64 "eOCgSyQw+fumSQsdVHqHMjDhQWgvHV6tTsPDie/kMs3KJPfPn0xycuyFiTq7ig6s3D4TBmWWKQqJEqfr/A0nHcOeLVI+TCPNTE4q+6AHvVLmVYjoSV61k8Vpn6b5ixm4lmlACE5RLUYymC2xKLT6AGnZPToeJLwOUX9quPKAIoOCObJh3EFE3il6ZBeeAYxh+hdXOPdZ2O4Zkqn5HS8IMUhrlc24CXSUqjrsI+V7tB+qBmR33O+1wZ1+nLUgKRhPXDf0p/SbfQddnQmESrZn/ylnl+Ua6R81hX40mKi9O2HXshMLD9j9IuA337ESheMKGMPRuHlzSdpq/0DM3zSybKzjmyDmiZ0qX6OF+6E7qR4Ss+7nbW1vWl1q2rk1p/n4GJ8DYIxKyaq4lPqJPePmGXF2HUmwyJFj85l3QdWekzH9yu+yrPpDz3zp0L5+2T2gtPsQ7zKTHyjedMAbwtBfUPTOvWSb1X+hXw14RiAF6Xf6lMsZNvIGQOp7ag/bo4l1DwBJul7J9gFH6sHe6dvo9AzCHHMVuJR4n0ikUql3wFc73ydRv5lkvVoVbkxsRBeJWvDXGbhy50K1UBh+tID/MWRUjX91RUYzyrKpTvB+BU8Q8gnbeTb7oCb+Vn1T3rEpnwVNGvSOCoFUwe29J6WBt5HcybL7Z0a0O0jbNqebJI8=")))
