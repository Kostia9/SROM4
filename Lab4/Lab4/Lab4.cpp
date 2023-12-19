#include <bitset>
#include <iostream>
#include <cassert>
#include <chrono>
#include <random>
#include <string>

const int m = 173;

using MultiplicativeMatrix = std::vector<std::bitset<m>>;

int custom_clz(uint32_t x) {
    if (x == 0) return 32; // якщо чиcло 0, повертаємо 32 (вcі біти нульові)

    int leading_zeros = 0;
    for (int i = 31; i >= 0; --i) {
        if ((x & (static_cast<uint32_t>(1) << i)) == 0) {
            leading_zeros++;
        }
        else {
            break;
        }
    }
    return leading_zeros;
}

int mod_pow2(int exponent, int mod) {
    int result = 1;
    for (int i = 0; i < exponent; ++i) {
        result = (result << 1) % mod;
    }
    return result;
}


class GF2m {
private:
    std::bitset<m> value;
    static MultiplicativeMatrix multiplicativeMatrix;

public:
    GF2m() : value(0) {}

    GF2m(const std::bitset<m>& val) : value(val) {}

    GF2m(const std::string& str, bool isHex = 0) {
        *this = isHex ? fromHex(str) : fromString(str);
    }

    static void calculateMultiplicativeMatrix() {
        int p = 2 * m + 1;

        for (int i = 0; i < m; ++i) {
            for (int j = 0; j < m; ++j) {
                int two_i = mod_pow2(i, p);
                int two_j = mod_pow2(j, p);

                if ((two_i + two_j) % p == 1 ||
                    (two_i - two_j + p) % p == 1 ||
                    ((p - two_i) + two_j) % p == 1 ||
                    ((p - two_i) - two_j + p) % p == 1) {
                    multiplicativeMatrix[m - i - 1][m - j - 1] = 1;
                }
            }
        }
    }

    GF2m add(const GF2m& other) const {
        return GF2m(value ^ other.value);
    }

    GF2m multiply(const GF2m& other) const {
        std::bitset<m> z;
        for (int i = 0; i < m; ++i) {
            std::bitset<m> u = (value << i) | (value >> (m - i));
            std::bitset<m> v = (other.value << i) | (other.value >> (m - i));
            std::bitset<m> u_times_matrix;
            // Множимо u на multiplicativeMatrix
            for (int j = 0; j < m; ++j) {
                if (u.test(j)) {
                    for (int k = 0; k < m; ++k) {
                        if (multiplicativeMatrix[j].test(k)) {
                            u_times_matrix.flip(k);
                        }
                    }
                }
            }
            // Домножаємо на v
            for (int j = 0; j < m; ++j) {
                if (u_times_matrix.test(j) && v.test(j))
                    z.flip(m - i - 1);
            }
        }
        return GF2m(z);
    }

    GF2m square() const {
        return GF2m((value >> 1) | (value << (m - 1)));
    }

    GF2m pow(const GF2m& power) const {
        GF2m result("1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 1);
        GF2m base = *this;

        for (int i = 0; i < m; ++i) {
            if (power.value.test(i)) {
                result = result.multiply(base);
            }
            base = base.square();
        }
        return result;
    }

    int trace() const {
        return value.count()%2;
    }

    GF2m inverse() const {
        if (this->value.none()) {
            throw std::runtime_error("Неможливо знайти обернений елемент до нуля");
        }
        int t = 31 - custom_clz(m - 1);
        GF2m a = *this;
        GF2m b = a;
        GF2m tmp;
        int k = 1;
        for (int i = t - 1; i >= 0; --i) {
            tmp = b;
            for (int j = 0; j < k; j++) b = b.square();
            b = b * tmp;
            k = 2 * k;
            if ((m-1) & (1 << i)) {
                b = b.square() * a;
                k = k + 1;
            }
        }
        b = b.square();
        return b;
    }

    std::string toString() const {
        std::string str;
        for (int i = m - 1; i >= 0; --i) {
            str += (value.test(i) ? '1' : '0');
        }
        return str;
    }

    static GF2m fromString(const std::string& str) {
        std::string result = str;
        if (str.size() > m) {
            throw std::invalid_argument("Рядок має неправильну довжину");
        }
        if (str.size() < m) {
            // Доповнюємо рядок нулями зліва
            result = std::string(m - str.size(), '0') + str;
        }
        for (char c : str) {
            if (c != '0' && c != '1') {
                throw std::invalid_argument("Неправильний символ у рядку");
            }
        }
        return GF2m(std::bitset<m>(result));
    }

    std::string toHex() const {
        std::string hexStr;

        for (int i = 0; i < m; i += 4) {
            int hexValue = 0;
            for (int j = 0; j < 4 && (i + j) < m; ++j) {
                if (value.test(i + j)) {
                    hexValue |= 1 << j;
                }
            }

            char hexChar = hexValue < 10 ? '0' + hexValue : 'A' + hexValue - 10;
            hexStr = hexChar + hexStr;
        }

        // Видалення лідуючих нулів
        size_t startPos = hexStr.find_first_not_of('0');
        if (startPos != std::string::npos) {
            hexStr = hexStr.substr(startPos);
        }
        else {
            hexStr = "0"; // Всі біти були нулями
        }

        return hexStr;
    }

    static GF2m fromHex(const std::string& hexStr) {
        std::string binStr;
        for (char hexChar : hexStr) {
            int hexValue;
            if (hexChar >= '0' && hexChar <= '9') {
                hexValue = hexChar - '0';
            }
            else if (hexChar >= 'A' && hexChar <= 'F') {
                hexValue = hexChar - 'A' + 10;
            }
            else if (hexChar >= 'a' && hexChar <= 'f') {
                hexValue = hexChar - 'a' + 10;
            }
            else {
                hexValue = -1; // Неправильний символ
            }

            if (hexValue == -1) {
                throw std::invalid_argument("Неправильний символ у шістнадцятковому рядку");
            }
            for (int i = 3; i >= 0; --i) {
                binStr += (hexValue & (1 << i)) ? '1' : '0';
            }
        }
        // Обрізаємо старші нулі, якщо це необхідно
        auto firstNonZero = binStr.find_first_not_of('0');
        if (firstNonZero != std::string::npos) {
            binStr = binStr.substr(firstNonZero);
        }
        if (binStr.size() > m) {
            throw std::invalid_argument("Шістнадцятковий рядок занадто довгий");
        }
        // Доповнюємо рядок нулями зліва, якщо він коротший за m
        if (binStr.size() < m) {
            binStr = std::string(m - binStr.size(), '0') + binStr;
        }
        return GF2m(std::bitset<m>(binStr));
    }

    void print() const {
        for (int i = m - 1; i >= 0; --i) {
            std::cout << value.test(i);
        }
        std::cout << std::endl;
    }

    GF2m& operator=(const GF2m& other) {
        if (this != &other) {
            this->value = other.value;
        }
        return *this;
    }

    bool operator==(const GF2m& other) const {
        return value == other.value;
    }

    GF2m operator+(const GF2m& other) const {
        return this->add(other);
    }

    GF2m operator*(const GF2m& other) const {
        return this->multiply(other);
    }

    static void printMultiplicativeMatrix() {
        for (int i = 0; i < m; ++i) {
            for (int j = 0; j < m; ++j) {
                std::cout << multiplicativeMatrix[i][j];
            }
            std::cout << std::endl;
        }
    }
};


void testAddition() {
    GF2m A1("01010000010111000001000101001010111010000100111100100000100110000100010101000001010110111110001101111101101101100101111001110110100011111011000001111101001111011011010010011");
    GF2m B1("01001001111011010100111010001010100001100000000110011011100010110000011100001000101011011110101001010011101111000110011100100001101101110000111000101010011000111011110011111");
    assert(A1 + B1 == GF2m("00011001101100010101111111000000011011100100111010111011000100110100001001001001111101100000100100101110000010100011100101010111001110001011111001010111010111100000100001100"));

    GF2m A2("00101110101111100010010110001000001101101101100111001101010011111100011100011011111000100000010101101001010010011110101111010101010000101101100101110010111011001010011000100");
    GF2m B2("01101110101010111011100111101000101110110110010011100000011100011100111110010000000011111001000001001011000000111000000011001110011110101010001100110011100000111011011101011");
    assert(A2 + B2 == GF2m("01000000000101011001110001100000100011011011110100101101001111100000100010001011111011011001010100100010010010100110101100011011001110000111101001000001011011110001000101111"));

    GF2m A3("0AE91DB7FBD1EBAC661F6488CC27F208C2B136493261", 1);
    GF2m B3("0D5026BF220F27A2D765193E6C14502E37F19293A040", 1);
    assert(A3 + B3 == GF2m("07B93B08D9DECC0EB17A7DB6A033A226F540A4DA9221", 1));

    GF2m A4("17182F40654A23682F00C3790B2E6714CE97F804BFB4", 1);
    GF2m B4("08A7A2AAFB1EE180992EF7BD70265A48086F4B84842D", 1);
    assert(A4 + B4 == GF2m("1FBF8DEA9E54C2E8B62E34C47B083D5CC6F8B3803B99", 1));

    GF2m A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 1);
    GF2m B5("ABCDFAFACBACFACBACFACBACFACB", 1);
    assert(A5 + B5 == GF2m("ABCDE5171170467110467073724073724", 1));
}
void testMultiplication() {
    GF2m A1("01010000010111000001000101001010111010000100111100100000100110000100010101000001010110111110001101111101101101100101111001110110100011111011000001111101001111011011010010011");
    GF2m B1("01001001111011010100111010001010100001100000000110011011100010110000011100001000101011011110101001010011101111000110011100100001101101110000111000101010011000111011110011111");
    assert(A1 * B1 == GF2m("00110110010010111111111110011011111000011000010101101111011000110110001001011011110000100111100000000011100100011011111000110011101101010010100111010110000011111111011110001"));

    GF2m A2("00101110101111100010010110001000001101101101100111001101010011111100011100011011111000100000010101101001010010011110101111010101010000101101100101110010111011001010011000100");
    GF2m B2("01101110101010111011100111101000101110110110010011100000011100011100111110010000000011111001000001001011000000111000000011001110011110101010001100110011100000111011011101011");
    assert(A2 * B2 == GF2m("00010010110010001101111100010111000011111101111011111000110111100010111100110101011110010101010111001111110001111010000100011010011110001011011001110000000001110010100101110"));

    GF2m A3("0AE91DB7FBD1EBAC661F6488CC27F208C2B136493261", 1);
    GF2m B3("0D5026BF220F27A2D765193E6C14502E37F19293A040", 1);
    assert(A3 * B3 == GF2m("04B6D8DE392424850992E85DD6D258449A398E6ACA92", 1));

    GF2m A4("17182F40654A23682F00C3790B2E6714CE97F804BFB4", 1);
    GF2m B4("08A7A2AAFB1EE180992EF7BD70265A48086F4B84842D", 1);
    assert(A4 * B4 == GF2m("1FED401A3B3EDB2752315B4D38AF2556B9209DB1C927", 1));

    GF2m A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 1);
    GF2m B5("ABCDFAFACBACFACBACFACBACFACB", 1);
    assert(A5 * B5 == GF2m("175EB77F6B38BC203C918B26A1BE56670C516D161455", 1));
}
void testTrace() {
    GF2m A1("0101000001011100000100010100101011101000010011100100000100110000100010101000001010110111110001101111101101101100101111001110110100011111011000001111101001111011011010010011");
    assert(A1.trace() == 1);

    GF2m A2("00101110101111100010010110001000001101101101100111001101010011111100011100011011111000100000010101101001010010011110101111010101010000101101100101110010111011001010011000100");
    assert(A2.trace() == 0);

    GF2m A3("0AE91DB7FBD1EBAC661F6488CC27F208C2B136493261", 1);
    assert(A3.trace() == 0);

    GF2m A4("17182F40654A23682F00C3790B2E6714CE97F804BFB4", 1);
    assert(A4.trace() == 0);

    GF2m A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 1);
    assert(A5.trace() == 0);
}
void testSquare() {
    GF2m A1("01010000010111000001000101001010111010000100111100100000100110000100010101000001010110111110001101111101101101100101111001110110100011111011000001111101001111011011010010011");
    assert(A1.square() == GF2m("10101000001011100000100010100101011101000010011110010000010011000010001010100000101011011111000110111110110110110010111100111011010001111101100000111110100111101101101001001"));

    GF2m A2("00101110101111100010010110001000001101101101100111001101010011111100011100011011111000100000010101101001010010011110101111010101010000101101100101110010111011001010011000100");
    assert(A2.square() == GF2m("00010111010111110001001011000100000110110110110011100110101001111110001110001101111100010000001010110100101001001111010111101010101000010110110010111001011101100101001100010"));

    GF2m A3("0AE91DB7FBD1EBAC661F6488CC27F208C2B136493261", 1);
    assert(A3.square() == GF2m("15748EDBFDE8F5D6330FB2446613F90461589B249930", 1));

    GF2m A4("17182F40654A23682F00C3790B2E6714CE97F804BFB4", 1);
    assert(A4.square() == GF2m("0B8C17A032A511B4178061BC8597338A674BFC025FDA", 1));

    GF2m A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 1);
    assert(A5.square() == GF2m("1000000000055E6F7D5E76FF565EFF5655E6F7D5E6F7", 1));
}
void testPow() {
    GF2m A1("01010000010111000001000101001010111010000100111100100000100110000100010101000001010110111110001101111101101101100101111001110110100011111011000001111101001111011011010010011");
    GF2m B1("01001001111011010100111010001010100001100000000110011011100010110000011100001000101011011110101001010011101111000110011100100001101101110000111000101010011000111011110011111");
    assert(A1.pow(B1) == GF2m("11111100101100000110000001100010001001110000001101000110111110111010011010000101100101011000110001111011000011010111001001011011111110011111001010101100000101111111110100001"));

    GF2m A2("00101110101111100010010110001000001101101101100111001101010011111100011100011011111000100000010101101001010010011110101111010101010000101101100101110010111011001010011000100");
    GF2m B2("01101110101010111011100111101000101110110110010011100000011100011100111110010000000011111001000001001011000000111000000011001110011110101010001100110011100000111011011101011");
    assert(A2.pow(B2) == GF2m("10001000011010011001101010001100011111001011101010001101111100111110110110100110111110010001001001011111110001101110101100101100001101001101100010100100111110001001100001011"));

    GF2m A3("0AE91DB7FBD1EBAC661F6488CC27F208C2B136493261", 1);
    GF2m B3("0D5026BF220F27A2D765193E6C14502E37F19293A040", 1);
    assert(A3.pow(B3) == GF2m("194BC5F43C7CB6272885A87760EFA918B4A0C2A90E9F", 1));

    GF2m A4("17182F40654A23682F00C3790B2E6714CE97F804BFB4", 1);
    GF2m B4("08A7A2AAFB1EE180992EF7BD70265A48086F4B84842D", 1);
    assert(A4.pow(B4) == GF2m("1F48B2B63E96D443EDA95F0ED95B8D1B0FE6B0DEBAF8", 1));

    GF2m A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 1);
    GF2m B5("ABCDFAFACBACFACBACFACBACFACB", 1);
    assert(A5.pow(B5) == GF2m("06D515A93DCA0D686E6547B26608D78D24FACFF12FE2", 1));
}
void testInverse() {
    GF2m A1("01010000010111000001000101001010111010000100111100100000100110000100010101000001010110111110001101111101101101100101111001110110100011111011000001111101001111011011010010011");
    assert(A1.inverse() == GF2m("11101101110101011000110000011011111000110101001100110101010001101101111100110101110101100101000101101101011010000000001101100011110100101100000001010101101001000111001100111"));

    GF2m A2("00101110101111100010010110001000001101101101100111001101010011111100011100011011111000100000010101101001010010011110101111010101010000101101100101110010111011001010011000100");
    assert(A2.inverse() == GF2m("11000000011111011110000000001101110100111001110101111000111100010001001000001010011100000010010110111011010001000001110011111100001100011111100111111010011100000100110111111"));

    GF2m A3("0AE91DB7FBD1EBAC661F6488CC27F208C2B136493261", 1);
    assert(A3.inverse() == GF2m("0250F3C885BEAC3D7CFA993BDB13ED26E409392FEDC2", 1));

    GF2m A4("17182F40654A23682F00C3790B2E6714CE97F804BFB4", 1);
    assert(A4.inverse() == GF2m("10B00FD70CED17CEF0200462504CBF3478D6B2C6B9DE", 1));

    GF2m A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 1);
    assert(A5.inverse() == GF2m("03F8E7199B0CCF8AA5167D40076A2F1755D52CC5238A", 1));
}
void otherTests() {
    std::bitset<173> bitset;
    bitset.set(); // Встановлює всі біти у 1
    GF2m Max(bitset);
    GF2m a("1E5908188E2D4E112EC2B9F5EBDBE7703651A1A520DC", 1);
    GF2m b("0ACE832553C6A2574E988BD34ED55D7918BEDFC36474", 1);
    GF2m c("008B945DE1FD63598C82DC661E9EB94757153572503E", 1);

    //(a+b)c=c(a+b)=ac+bc
    assert((a + b) * c == a * c + b * c);
    assert((a + b) * c == c * (a + b));
    //a^Max=1
    assert(a.pow(Max) == GF2m("1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 1));
}


std::string generateRandomNumberString(int length) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(0, 1); // generating digits between 0 and 9

    std::string result;
    for (int i = 0; i < length; ++i) {
        result += std::to_string(dis(gen));
    }
    return result;
}
void timeTest() {

    GF2m numbers[20];
    for (int i = 0; i < 20; i++) {
        GF2m temp(generateRandomNumberString(173));
        numbers[i] = temp;
    }
    GF2m a;
    int b;
    auto start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 1001; i++) {
        a = numbers[i % 20] + numbers[(i - 1) % 20];
    }
    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for addition: " << duration.count() << " microseconds." << std::endl;


    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 101; i++) {
        a = numbers[i % 20] * numbers[(i - 1) % 20];
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for multiplication: " << duration.count() << " microseconds." << std::endl;

    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 1001; i++) {
        b = numbers[i % 20].trace();
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for trace: " << duration.count() << " microseconds." << std::endl;

    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 1001; i++) {
        a = numbers[i % 20].square();
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for square: " << duration.count() << " microseconds." << std::endl;


    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 11; i++) {
        a = numbers[i % 20].pow(numbers[(i - 1) % 20]);
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for pow: " << duration.count() << " microseconds." << std::endl;



    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 11; i++) {
        a = numbers[i % 20].inverse();
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for inverse: " << duration.count() << " microseconds." << std::endl;

}





MultiplicativeMatrix GF2m::multiplicativeMatrix(m);
int main() {
    GF2m::calculateMultiplicativeMatrix();

    /*timeTest();
    testAddition();
    testMultiplication();
    testSquare();
    testInverse();
    testPow();
    otherTests();
    testTrace();
    std::cout << "Всі тести пройшли успішно!\n";*/
    
    GF2m a("1C610FA72B082905FB7D0597D754DDF64330A6E38D87", 1);
    GF2m b("1322645130758ED775F543D63398E11C2FC9BD377E4E", 1);
    GF2m c("1311EF56624F81C6C43609B74687D8BAF7E0916BDD1E", 1);
    /*std::cout << "a: " << (a).toHex() << std::endl;

    std::cout << "Сума: " << (a.add(b)).toHex() << std::endl;
    std::cout << "Добуток: " << (a.multiply(b)).toHex() << std::endl;
    std::cout << "Квадрат a^2: " << (a.square()).toHex() << std::endl;
    std::cout << "Time taken for mult: " << duration.count() / 10 << " microseconds." << std::endl;

    std::cout << "Слід a: " << a.trace() << std::endl;
    std::cout << "Обернений a^-1: " << a.inverse().toHex() << std::endl;
    std::cout << "a*a^-1: " << a.multiply(a.inverse()).toHex() << std::endl;

    std::cout << "Степінь a^c: " << (a.pow(c)).toHex() << std::endl;*/
    


    return 0;
}