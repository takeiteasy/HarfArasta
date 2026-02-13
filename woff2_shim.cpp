#include <cstdlib>
#include <cstdint>
#include <woff2/decode.h>
#include <woff2/output.h>

extern "C" {

uint8_t* woff2_decode(const uint8_t* data, size_t length, size_t* out_len) {
    size_t final_size = woff2::ComputeWOFF2FinalSize(data, length);
    if (final_size == 0)
        return nullptr;

    uint8_t* buf = static_cast<uint8_t*>(std::malloc(final_size));
    if (!buf)
        return nullptr;

    woff2::WOFF2MemoryOut out(buf, final_size);
    if (!woff2::ConvertWOFF2ToTTF(data, length, &out)) {
        std::free(buf);
        return nullptr;
    }

    *out_len = out.Size();
    return buf;
}

void woff2_decode_free(uint8_t* buf) {
    std::free(buf);
}

}
