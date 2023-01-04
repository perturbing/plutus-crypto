# plutus-crypto
A naive implementation of a twisted Ed25519 curve in Plutus for fun. This implementation is naive since it implements the twisted Edward curve over the finite field `F_q` (q = 2^255-19) in a mathematical correct but inefficient way.

# Disclaimer
This is not an audited, use at your own risk!

# Possible improvements
From the source of these improvements see the original DJB paper "High speed High-security signatures"

- Use bitwise [primitives](https://github.com/cardano-foundation/CIPs/pull/283) and Redix 64 notation of the field to improve the speed of multiplication of points
- Represent group points in their projective plane form to prevent the usage of the costly field inversion in point addition

# Possible extensions

- Add El Gamal and some ZK proofs (DLOG and DLEQ) and possibly a proof of correct [shuffle](http://www0.cs.ucl.ac.uk/staff/J.Groth/MinimalShuffle.pdf) (though I do not know how succinct this proof is)
- Add an EC [VRF](https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-vrf-15#name-elliptic-curve-vrf-ecvrf). It would be cool to have onchain randomness generated this way as [chainlink](https://chain.link/vrf) does
