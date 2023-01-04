# plutus-crypto
A naive implementation of a twisted Ed25519 curve in Plutus for fun.

# Disclaimer
This is not an audited, use at your own risk!

# Possible improvements
From the source of these improvements see the original DJB paper "High speed High-security signatures"

- Use bit wise primitives and Redix 64 notation of the field to improve the speed of multiplication of points
- Represent group points in their projective plane form to prevent the usage of the costly field inversion in point addition
