# Lexer Test: `numbers`

## Errors
- (1:3-2:1) We want a binary digit but the line ends.
- (2:3-3:1) We want a binary digit but the line ends.
- (3:3-4:1) We want a binary digit but the line ends.
- (4:3-5:1) We want a binary digit but the line ends.
- (5:3-5:4) We want a binary digit but we have `2`.
- (6:3-6:4) We want a binary digit but we have `2`.
- (7:3-7:4) We want a binary digit but we have `c`.
- (8:3-8:4) We want a binary digit but we have `c`.
- (9:3-9:5) We want a binary digit but we have `😈`.
- (10:3-10:5) We want a binary digit but we have `😈`.
- (23:4-23:5) We want a binary digit but we have `2`.
- (24:4-24:5) We want a binary digit but we have `x`.
- (25:4-25:6) We want a binary digit but we have `𐐷`.
- (26:4-26:5) We want a binary digit but we have `x`.
- (27:4-27:5) We want a binary digit but we have `2`.
- (28:4-28:6) We want a binary digit but we have `𐐷`.
- (29:4-29:5) We want a binary digit but we have `x`.
- (30:4-30:5) We want a binary digit but we have `x`.
- (31:4-31:5) We want a binary digit but we have `2`.
- (32:3-32:4) We want a binary digit but we have `2`.
- (33:3-33:4) We want a binary digit but we have `x`.
- (34:3-34:5) We want a binary digit but we have `𐐷`.
- (35:3-35:4) We want a binary digit but we have `x`.
- (36:3-36:4) We want a binary digit but we have `2`.
- (37:3-37:5) We want a binary digit but we have `𐐷`.
- (38:3-38:4) We want a binary digit but we have `x`.
- (39:3-39:4) We want a binary digit but we have `x`.
- (40:3-40:4) We want a binary digit but we have `2`.
- (44:4-44:5) We want a binary digit but we have `x`.
- (45:3-46:1) We want a hexadecimal digit but the line ends.
- (73:3-74:1) We want a hexadecimal digit but the line ends.
- (101:3-101:4) We want a hexadecimal digit but we have `x`.
- (102:3-102:5) We want a hexadecimal digit but we have `𐐷`.
- (103:3-103:4) We want a hexadecimal digit but we have `x`.
- (104:3-104:5) We want a hexadecimal digit but we have `𐐷`.
- (105:4-105:5) We want a hexadecimal digit but we have `x`.
- (106:4-106:6) We want a hexadecimal digit but we have `𐐷`.
- (107:4-107:5) We want a hexadecimal digit but we have `x`.
- (108:4-108:6) We want a hexadecimal digit but we have `𐐷`.
- (166:3-167:1) We want a number but the line ends.
- (167:3-168:1) We want a number but the line ends.
- (168:4-169:1) We want a number but the line ends.
- (169:4-170:1) We want a number but the line ends.
- (170:4-171:1) We want a number but the line ends.
- (171:4-172:1) We want a number but the line ends.
- (182:5-183:1) We want a number but the line ends.
- (184:4-185:1) We want a number but the line ends.
- (185:4-186:1) We want a number but the line ends.
- (188:6-189:1) We want a number but the line ends.
- (189:6-190:1) We want a number but the line ends.
- (192:3-192:4) We want a number but we have `x`.
- (193:4-193:5) We want a number but we have `x`.
- (194:2-194:3) We want a number but we have `x`.
- (195:3-195:4) We want a number but we have `x`.
- (196:4-196:5) We want a number but we have `x`.
- (197:5-197:6) We want a number but we have `x`.
- (198:6-198:7) We want a number but we have `x`.
- (199:3-199:4) We want a number but we have `x`.
- (200:4-200:5) We want a number but we have `x`.
- (201:4-201:5) We want a number but we have `x`.
- (202:4-202:5) We want a number but we have `x`.
- (203:5-203:6) We want a number but we have `x`.
- (204:5-204:6) We want a number but we have `x`.
- (213:2-213:3) We want a number but we have `p`.
- (214:4-214:5) We want a number but we have `p`.
- (215:4-215:5) We want a number but we have `p`.

## Tokens
| Range          | Kind                           | Data                       |
|----------------|--------------------------------|----------------------------|
| 1:1-1:3        | Number::Invalid                | 0b                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 2:1-2:3        | Number::Invalid                | 0B                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 3:1-3:3        | Number::Invalid                | 0b                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 4:1-4:3        | Number::Invalid                | 0B                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 5:1-5:4        | Number::Invalid                | 0b2                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 6:1-6:4        | Number::Invalid                | 0B2                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 7:1-7:4        | Number::Invalid                | 0bc                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 8:1-8:4        | Number::Invalid                | 0Bc                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 9:1-9:3        | Number::Invalid                | 0b                         |
| 9:3-9:5        | UnexpectedChar                 | `😈`                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 10:1-10:3      | Number::Invalid                | 0B                         |
| 10:3-10:5      | UnexpectedChar                 | `😈`                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 11:1-11:4      | Number::BinaryInteger          | 0                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 12:1-12:4      | Number::BinaryInteger          | 0                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 13:1-13:4      | Number::BinaryInteger          | 1                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 14:1-14:4      | Number::BinaryInteger          | 1                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 15:1-15:5      | Number::BinaryInteger          | 1                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 16:1-16:5      | Number::BinaryInteger          | 1                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 17:1-17:5      | Number::BinaryInteger          | 10                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 18:1-18:5      | Number::BinaryInteger          | 10                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 19:1-19:15     | Number::BinaryInteger          | 101010100011               |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 20:1-20:15     | Number::BinaryInteger          | 101010100011               |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 21:1-21:15     | Number::BinaryInteger          | 1010111                    |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 22:1-22:15     | Number::BinaryInteger          | 1010111                    |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 23:1-23:5      | Number::Invalid                | 0b02                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 24:1-24:5      | Number::Invalid                | 0b0x                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 25:1-25:6      | Number::Invalid                | 0b0𐐷                    |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 26:1-26:6      | Number::Invalid                | 0b0x2                      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 27:1-27:6      | Number::Invalid                | 0b02x                      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 28:1-28:7      | Number::Invalid                | 0b0𐐷x                   |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 29:1-29:7      | Number::Invalid                | 0b0x𐐷                   |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 30:1-30:8      | Number::Invalid                | 0b0x𐐷x                  |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 31:1-31:8      | Number::Invalid                | 0b02𐐷2                  |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 32:1-32:4      | Number::Invalid                | 0b2                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 33:1-33:4      | Number::Invalid                | 0bx                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 34:1-34:5      | Number::Invalid                | 0b𐐷                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 35:1-35:5      | Number::Invalid                | 0bx2                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 36:1-36:5      | Number::Invalid                | 0b2x                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 37:1-37:6      | Number::Invalid                | 0b𐐷x                    |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 38:1-38:6      | Number::Invalid                | 0bx𐐷                    |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 39:1-39:7      | Number::Invalid                | 0bx𐐷x                   |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 40:1-40:7      | Number::Invalid                | 0b2𐐷2                   |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 41:1-41:4      | Number::BinaryInteger          | 0                          |
| trailing       | Trivia::Spaces                 | 1                          |
| 41:5-41:6      | Identifier                     | `x`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 42:1-42:2      | Identifier                     | `x`                        |
| trailing       | Trivia::Spaces                 | 1                          |
| 42:3-42:6      | Number::BinaryInteger          | 0                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 43:1-43:5      | Identifier                     | `x0b0`                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 44:1-44:5      | Number::Invalid                | 0b0x                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 45:1-45:3      | Number::Invalid                | 0x                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 46:1-46:4      | Number::HexadecimalInteger     | 0                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 47:1-47:4      | Number::HexadecimalInteger     | 1                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 48:1-48:4      | Number::HexadecimalInteger     | 2                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 49:1-49:4      | Number::HexadecimalInteger     | 3                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 50:1-50:4      | Number::HexadecimalInteger     | 4                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 51:1-51:4      | Number::HexadecimalInteger     | 5                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 52:1-52:4      | Number::HexadecimalInteger     | 6                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 53:1-53:4      | Number::HexadecimalInteger     | 7                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 54:1-54:4      | Number::HexadecimalInteger     | 8                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 55:1-55:4      | Number::HexadecimalInteger     | 9                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 56:1-56:4      | Number::HexadecimalInteger     | A                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 57:1-57:4      | Number::HexadecimalInteger     | B                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 58:1-58:4      | Number::HexadecimalInteger     | C                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 59:1-59:4      | Number::HexadecimalInteger     | D                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 60:1-60:4      | Number::HexadecimalInteger     | E                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 61:1-61:4      | Number::HexadecimalInteger     | F                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 62:1-62:4      | Number::HexadecimalInteger     | A                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 63:1-63:4      | Number::HexadecimalInteger     | B                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 64:1-64:4      | Number::HexadecimalInteger     | C                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 65:1-65:4      | Number::HexadecimalInteger     | D                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 66:1-66:4      | Number::HexadecimalInteger     | E                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 67:1-67:4      | Number::HexadecimalInteger     | F                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 68:1-68:6      | Number::HexadecimalInteger     | FFF                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 69:1-69:9      | Number::HexadecimalInteger     | C0FFEE                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 70:1-70:9      | Number::HexadecimalInteger     | C0FFEE                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 71:1-71:9      | Number::HexadecimalInteger     | C0FF33                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 72:1-72:9      | Number::HexadecimalInteger     | C0FF33                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 73:1-73:3      | Number::Invalid                | 0X                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 74:1-74:4      | Number::HexadecimalInteger     | 0                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 75:1-75:4      | Number::HexadecimalInteger     | 1                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 76:1-76:4      | Number::HexadecimalInteger     | 2                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 77:1-77:4      | Number::HexadecimalInteger     | 3                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 78:1-78:4      | Number::HexadecimalInteger     | 4                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 79:1-79:4      | Number::HexadecimalInteger     | 5                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 80:1-80:4      | Number::HexadecimalInteger     | 6                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 81:1-81:4      | Number::HexadecimalInteger     | 7                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 82:1-82:4      | Number::HexadecimalInteger     | 8                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 83:1-83:4      | Number::HexadecimalInteger     | 9                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 84:1-84:4      | Number::HexadecimalInteger     | A                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 85:1-85:4      | Number::HexadecimalInteger     | B                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 86:1-86:4      | Number::HexadecimalInteger     | C                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 87:1-87:4      | Number::HexadecimalInteger     | D                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 88:1-88:4      | Number::HexadecimalInteger     | E                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 89:1-89:4      | Number::HexadecimalInteger     | F                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 90:1-90:4      | Number::HexadecimalInteger     | A                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 91:1-91:4      | Number::HexadecimalInteger     | B                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 92:1-92:4      | Number::HexadecimalInteger     | C                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 93:1-93:4      | Number::HexadecimalInteger     | D                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 94:1-94:4      | Number::HexadecimalInteger     | E                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 95:1-95:4      | Number::HexadecimalInteger     | F                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 96:1-96:6      | Number::HexadecimalInteger     | FFF                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 97:1-97:9      | Number::HexadecimalInteger     | C0FFEE                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 98:1-98:9      | Number::HexadecimalInteger     | C0FFEE                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 99:1-99:9      | Number::HexadecimalInteger     | C0FF33                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 100:1-100:9    | Number::HexadecimalInteger     | C0FF33                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 101:1-101:4    | Number::Invalid                | 0xx                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 102:1-102:5    | Number::Invalid                | 0x𐐷                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 103:1-103:7    | Number::Invalid                | 0xxxxx                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 104:1-104:8    | Number::Invalid                | 0x𐐷xxx                  |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 105:1-105:5    | Number::Invalid                | 0x0x                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 106:1-106:6    | Number::Invalid                | 0x0𐐷                    |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 107:1-107:8    | Number::Invalid                | 0x0xxxx                    |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 108:1-108:9    | Number::Invalid                | 0x0𐐷xxx                 |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 109:1-109:2    | Identifier                     | `o`                        |
| 109:2-109:3    | Glyph                          | `.`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 110:1-110:2    | Number::DecimalInteger         | 0                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 111:1-111:3    | Number::DecimalInteger         | 0                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 112:1-112:2    | Number::DecimalInteger         | 1                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 113:1-113:3    | Number::DecimalInteger         | 10                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 114:1-114:2    | Number::DecimalInteger         | 1                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 115:1-115:2    | Number::DecimalInteger         | 2                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 116:1-116:2    | Number::DecimalInteger         | 3                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 117:1-117:2    | Number::DecimalInteger         | 4                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 118:1-118:2    | Number::DecimalInteger         | 5                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 119:1-119:2    | Number::DecimalInteger         | 6                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 120:1-120:2    | Number::DecimalInteger         | 7                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 121:1-121:2    | Number::DecimalInteger         | 8                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 122:1-122:2    | Number::DecimalInteger         | 9                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 123:1-123:3    | Number::DecimalInteger         | 42                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 124:1-124:17   | Number::DecimalInteger         | 9007199254740991           |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 125:1-125:17   | Number::DecimalInteger         | 9007199254740992           |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 126:1-126:17   | Number::DecimalInteger         | 9007199254740993           |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 127:1-127:17   | Number::DecimalInteger         | 9007199254740994           |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 128:1-128:17   | Number::DecimalInteger         | 9007199254740995           |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 129:1-129:18   | Number::Float                  | 9.007199254740991e15       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 130:1-130:18   | Number::Float                  | 9.007199254740992e15       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 131:1-131:18   | Number::Float                  | 9.007199254740992e15       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 132:1-132:18   | Number::Float                  | 9.007199254740994e15       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 133:1-133:18   | Number::Float                  | 9.007199254740996e15       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 134:1-134:21   | Number::DecimalInteger         | 18446744073709552000       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 135:1-135:21   | Number::DecimalInteger         | 18446744073709552001       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 136:1-136:21   | Number::DecimalInteger         | 18446744073709552002       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 137:1-137:21   | Number::DecimalInteger         | 18446744073709552003       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 138:1-138:21   | Number::DecimalInteger         | 18446744073709552004       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 139:1-139:21   | Number::DecimalInteger         | 18446744073709552005       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 140:1-140:22   | Number::Float                  | 1.8446744073709552e19      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 141:1-141:22   | Number::Float                  | 1.8446744073709552e19      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 142:1-142:22   | Number::Float                  | 1.8446744073709552e19      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 143:1-143:22   | Number::Float                  | 1.8446744073709552e19      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 144:1-144:22   | Number::Float                  | 1.8446744073709552e19      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 145:1-145:22   | Number::Float                  | 1.8446744073709552e19      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 146:1-146:2    | Glyph                          | `.`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 147:1-147:2    | Glyph                          | `.`                        |
| 147:2-147:3    | Identifier                     | `p`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 148:1-148:2    | Glyph                          | `.`                        |
| 148:2-148:3    | Number::DecimalInteger         | 0                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 149:1-149:7    | Number::Float                  | 3.1415                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 150:1-150:3    | Number::Float                  | 1                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 151:1-151:2    | Glyph                          | `.`                        |
| 151:2-151:3    | Number::DecimalInteger         | 1                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 152:1-152:4    | Number::Float                  | 0.1                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 153:1-153:4    | Number::Float                  | 1.1                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 154:1-154:4    | Number::Float                  | 1.2                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 155:1-155:4    | Number::Float                  | 1.3                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 156:1-156:4    | Number::Float                  | 1.4                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 157:1-157:4    | Number::Float                  | 1.5                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 158:1-158:4    | Number::Float                  | 1.6                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 159:1-159:4    | Number::Float                  | 1.7                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 160:1-160:4    | Number::Float                  | 1.8                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 161:1-161:4    | Number::Float                  | 1.9                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 162:1-162:3    | Identifier                     | `e1`                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 163:1-163:3    | Identifier                     | `E1`                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 164:1-164:4    | Number::Float                  | 10                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 165:1-165:4    | Number::Float                  | 10                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 166:1-166:3    | Number::Invalid                | 1e                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 167:1-167:3    | Number::Invalid                | 1E                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 168:1-168:4    | Number::Invalid                | 1e+                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 169:1-169:4    | Number::Invalid                | 1e-                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 170:1-170:4    | Number::Invalid                | 1E+                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 171:1-171:4    | Number::Invalid                | 1E-                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 172:1-172:5    | Number::Float                  | 10                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 173:1-173:5    | Number::Float                  | 0.1                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 174:1-174:5    | Number::Float                  | 10                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 175:1-175:5    | Number::Float                  | 0.1                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 176:1-176:5    | Number::Float                  | 1e90                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 177:1-177:9    | Number::Float                  | 314.15                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 178:1-178:4    | Number::Float                  | 300                        |
| 178:4-178:5    | Glyph                          | `.`                        |
| 178:5-178:9    | Number::DecimalInteger         | 1415                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 179:1-179:2    | Number::DecimalInteger         | 3                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 180:1-180:3    | Number::Float                  | 3                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 181:1-181:4    | Number::Float                  | 3.1                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 182:1-182:5    | Number::Invalid                | 3.1e                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 183:1-183:6    | Number::Float                  | 310                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 184:1-184:4    | Number::Invalid                | 3e+                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 185:1-185:4    | Number::Invalid                | 3e-                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 186:1-186:5    | Number::Float                  | 300                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 187:1-187:5    | Number::Float                  | 0.03                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 188:1-188:6    | Number::Invalid                | 3.1e+                      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 189:1-189:6    | Number::Invalid                | 3.1e-                      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 190:1-190:7    | Number::Float                  | 310                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 191:1-191:7    | Number::Float                  | 0.031                      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 192:1-192:4    | Number::Invalid                | 3ex                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 193:1-193:5    | Number::Invalid                | 3e2x                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 194:1-194:3    | Number::Invalid                | 3x                         |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 195:1-195:4    | Number::Invalid                | 3.x                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 196:1-196:5    | Number::Invalid                | 3.1x                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 197:1-197:6    | Number::Invalid                | 3.1ex                      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 198:1-198:7    | Number::Invalid                | 3.1e2x                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 199:1-199:4    | Number::Invalid                | 3ex                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 200:1-200:5    | Number::Invalid                | 3e2x                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 201:1-201:5    | Number::Invalid                | 3e+x                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 202:1-202:5    | Number::Invalid                | 3e-x                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 203:1-203:6    | Number::Invalid                | 3e+2x                      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 204:1-204:6    | Number::Invalid                | 3e-2x                      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 205:1-205:5    | Number::Float                  | 0                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 206:1-206:5    | Number::Float                  | 100                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 207:1-207:5    | Number::Float                  | 0                          |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 208:1-208:5    | Number::Float                  | 100                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 209:1-209:6    | Number::Float                  | inf                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 210:1-210:24   | Number::Float                  | 1.7976931348623157e308     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 211:1-211:24   | Number::Float                  | 1.7976931348623157e308     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 212:1-212:24   | Number::Float                  | inf                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 213:1-213:4    | Number::Invalid                | 1px                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 214:1-214:6    | Number::Invalid                | 1.1px                      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 215:1-215:6    | Number::Invalid                | 1e1px                      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 216:1          | End                            |                            |
