{-# LANGUAGE TemplateHaskell #-}

module BigRecord where

import Control.Lens

data Big = Big
  { _a0 :: Int , _a1 :: Int , _a2 :: Int , _a3 :: Int , _a4 :: Int , _a5 :: Int , _a6 :: Int , _a7 :: Int
  , _a8 :: Int , _a9 :: Int , _a10 :: Int , _a11 :: Int , _a12 :: Int , _a13 :: Int , _a14 :: Int , _a15 :: Int
  , _a16 :: Int , _a17 :: Int , _a18 :: Int , _a19 :: Int , _a20 :: Int , _a21 :: Int , _a22 :: Int , _a23 :: Int
  , _a24 :: Int , _a25 :: Int , _a26 :: Int , _a27 :: Int , _a28 :: Int , _a29 :: Int , _a30 :: Int , _a31 :: Int
  , _a32 :: Int , _a33 :: Int , _a34 :: Int , _a35 :: Int , _a36 :: Int , _a37 :: Int , _a38 :: Int , _a39 :: Int
  , _a40 :: Int , _a41 :: Int , _a42 :: Int , _a43 :: Int , _a44 :: Int , _a45 :: Int , _a46 :: Int , _a47 :: Int
  , _a48 :: Int , _a49 :: Int , _a50 :: Int , _a51 :: Int , _a52 :: Int , _a53 :: Int , _a54 :: Int , _a55 :: Int
  , _a56 :: Int , _a57 :: Int , _a58 :: Int , _a59 :: Int , _a60 :: Int , _a61 :: Int , _a62 :: Int , _a63 :: Int
  , _a64 :: Int , _a65 :: Int , _a66 :: Int , _a67 :: Int , _a68 :: Int , _a69 :: Int , _a70 :: Int , _a71 :: Int
  , _a72 :: Int , _a73 :: Int , _a74 :: Int , _a75 :: Int , _a76 :: Int , _a77 :: Int , _a78 :: Int , _a79 :: Int
  , _a80 :: Int , _a81 :: Int , _a82 :: Int , _a83 :: Int , _a84 :: Int , _a85 :: Int , _a86 :: Int , _a87 :: Int
  , _a88 :: Int , _a89 :: Int , _a90 :: Int , _a91 :: Int , _a92 :: Int , _a93 :: Int , _a94 :: Int , _a95 :: Int
  , _a96 :: Int , _a97 :: Int , _a98 :: Int , _a99 :: Int
  }

data Bigger = Bigger
  { _b0 :: Int , _b1 :: Int , _b2 :: Int , _b3 :: Int , _b4 :: Int , _b5 :: Int , _b6 :: Int , _b7 :: Int
  , _b8 :: Int , _b9 :: Int , _b10 :: Int , _b11 :: Int , _b12 :: Int , _b13 :: Int , _b14 :: Int , _b15 :: Int
  , _b16 :: Int , _b17 :: Int , _b18 :: Int , _b19 :: Int , _b20 :: Int , _b21 :: Int , _b22 :: Int , _b23 :: Int
  , _b24 :: Int , _b25 :: Int , _b26 :: Int , _b27 :: Int , _b28 :: Int , _b29 :: Int , _b30 :: Int , _b31 :: Int
  , _b32 :: Int , _b33 :: Int , _b34 :: Int , _b35 :: Int , _b36 :: Int , _b37 :: Int , _b38 :: Int , _b39 :: Int
  , _b40 :: Int , _b41 :: Int , _b42 :: Int , _b43 :: Int , _b44 :: Int , _b45 :: Int , _b46 :: Int , _b47 :: Int
  , _b48 :: Int , _b49 :: Int , _b50 :: Int , _b51 :: Int , _b52 :: Int , _b53 :: Int , _b54 :: Int , _b55 :: Int
  , _b56 :: Int , _b57 :: Int , _b58 :: Int , _b59 :: Int , _b60 :: Int , _b61 :: Int , _b62 :: Int , _b63 :: Int
  , _b64 :: Int , _b65 :: Int , _b66 :: Int , _b67 :: Int , _b68 :: Int , _b69 :: Int , _b70 :: Int , _b71 :: Int
  , _b72 :: Int , _b73 :: Int , _b74 :: Int , _b75 :: Int , _b76 :: Int , _b77 :: Int , _b78 :: Int , _b79 :: Int
  , _b80 :: Int , _b81 :: Int , _b82 :: Int , _b83 :: Int , _b84 :: Int , _b85 :: Int , _b86 :: Int , _b87 :: Int
  , _b88 :: Int , _b89 :: Int , _b90 :: Int , _b91 :: Int , _b92 :: Int , _b93 :: Int , _b94 :: Int , _b95 :: Int
  , _b96 :: Int , _b97 :: Int , _b98 :: Int , _b99 :: Int , _b100 :: Int , _b101 :: Int , _b102 :: Int , _b103 :: Int
  , _b104 :: Int , _b105 :: Int , _b106 :: Int , _b107 :: Int , _b108 :: Int , _b109 :: Int , _b110 :: Int , _b111 :: Int
  , _b112 :: Int , _b113 :: Int , _b114 :: Int , _b115 :: Int , _b116 :: Int , _b117 :: Int , _b118 :: Int , _b119 :: Int
  , _b120 :: Int , _b121 :: Int , _b122 :: Int , _b123 :: Int , _b124 :: Int , _b125 :: Int , _b126 :: Int , _b127 :: Int
  , _b128 :: Int , _b129 :: Int , _b130 :: Int , _b131 :: Int , _b132 :: Int , _b133 :: Int , _b134 :: Int , _b135 :: Int
  , _b136 :: Int , _b137 :: Int , _b138 :: Int , _b139 :: Int , _b140 :: Int , _b141 :: Int , _b142 :: Int , _b143 :: Int
  , _b144 :: Int , _b145 :: Int , _b146 :: Int , _b147 :: Int , _b148 :: Int , _b149 :: Int , _b150 :: Int , _b151 :: Int
  , _b152 :: Int , _b153 :: Int , _b154 :: Int , _b155 :: Int , _b156 :: Int , _b157 :: Int , _b158 :: Int , _b159 :: Int
  , _b160 :: Int , _b161 :: Int , _b162 :: Int , _b163 :: Int , _b164 :: Int , _b165 :: Int , _b166 :: Int , _b167 :: Int
  , _b168 :: Int , _b169 :: Int , _b170 :: Int , _b171 :: Int , _b172 :: Int , _b173 :: Int , _b174 :: Int , _b175 :: Int
  , _b176 :: Int , _b177 :: Int , _b178 :: Int , _b179 :: Int , _b180 :: Int , _b181 :: Int , _b182 :: Int , _b183 :: Int
  , _b184 :: Int , _b185 :: Int , _b186 :: Int , _b187 :: Int , _b188 :: Int , _b189 :: Int , _b190 :: Int , _b191 :: Int
  , _b192 :: Int , _b193 :: Int , _b194 :: Int , _b195 :: Int , _b196 :: Int , _b197 :: Int , _b198 :: Int , _b199 :: Int
  , _b200 :: Int , _b201 :: Int , _b202 :: Int , _b203 :: Int , _b204 :: Int , _b205 :: Int , _b206 :: Int , _b207 :: Int
  , _b208 :: Int , _b209 :: Int , _b210 :: Int , _b211 :: Int , _b212 :: Int , _b213 :: Int , _b214 :: Int , _b215 :: Int
  , _b216 :: Int , _b217 :: Int , _b218 :: Int , _b219 :: Int , _b220 :: Int , _b221 :: Int , _b222 :: Int , _b223 :: Int
  , _b224 :: Int , _b225 :: Int , _b226 :: Int , _b227 :: Int , _b228 :: Int , _b229 :: Int , _b230 :: Int , _b231 :: Int
  , _b232 :: Int , _b233 :: Int , _b234 :: Int , _b235 :: Int , _b236 :: Int , _b237 :: Int , _b238 :: Int , _b239 :: Int
  , _b240 :: Int , _b241 :: Int , _b242 :: Int , _b243 :: Int , _b244 :: Int , _b245 :: Int , _b246 :: Int , _b247 :: Int
  , _b248 :: Int , _b249 :: Int , _b250 :: Int , _b251 :: Int , _b252 :: Int , _b253 :: Int , _b254 :: Int , _b255 :: Int
  , _b256 :: Int , _b257 :: Int , _b258 :: Int , _b259 :: Int , _b260 :: Int , _b261 :: Int , _b262 :: Int , _b263 :: Int
  , _b264 :: Int , _b265 :: Int , _b266 :: Int , _b267 :: Int , _b268 :: Int , _b269 :: Int , _b270 :: Int , _b271 :: Int
  , _b272 :: Int , _b273 :: Int , _b274 :: Int , _b275 :: Int , _b276 :: Int , _b277 :: Int , _b278 :: Int , _b279 :: Int
  , _b280 :: Int , _b281 :: Int , _b282 :: Int , _b283 :: Int , _b284 :: Int , _b285 :: Int , _b286 :: Int , _b287 :: Int
  , _b288 :: Int , _b289 :: Int , _b290 :: Int , _b291 :: Int , _b292 :: Int , _b293 :: Int , _b294 :: Int , _b295 :: Int
  , _b296 :: Int , _b297 :: Int , _b298 :: Int , _b299 :: Int
  }

makeLensesWith (lensRules & generateRecordSyntax .~ True) ''Big
makeLensesWith (lensRules & generateRecordSyntax .~ True) ''Bigger
