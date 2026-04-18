const HOST = "127.0.0.1";
const PORT = 8080;

function buildLoginPacket(username: string, _skin: number /* will implemtn! */): Uint8Array {
  const textEncoder = new TextEncoder();
  const data = Uint8Array.from([
    0x00,
    0x00,
    0x01,
    ...textEncoder.encode(username.slice(0, 16)),
    ...[
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00,
    ],
  ]);
  const [hi, low] = [(data.length >> 8) & 0xff, data.length & 0xff];
  console.log(hi.toString(16), low.toString(16));
  data[0] = hi;
  data[1] = low;
  console.log(data);
  return data;
}

function buildLocationPacket(isStreetFighter: boolean, mapId: number): Uint8Array {
  const payload = new Uint8Array(1);
  const modeBit = isStreetFighter ? 1 : 0;
  payload[0] = (modeBit << 7) | (mapId & 0x7f);
  const data = new Uint8Array([0x00, 0x00, 0x02, ...payload]);
  const [hi, low] = [(data.length >> 8) & 0xff, data.length & 0xff];
  console.log(hi.toString(16), low.toString(16));
  data[0] = hi;
  data[1] = low;
  console.log(data);
  return data;
}

function buildErrorTestPacket(): Uint8Array {
  return new Uint8Array([0xba, 0xad, 0xf0, 0x0d]);
}

// Start Bun TCP Client
const socket = await Bun.connect({
  hostname: HOST,
  port: PORT,
  socket: {
    data(socket, data) {
      console.log(data);
    },
    open(socket) {
      console.log("client connected");

      console.log("sending login (0x01) packet");
      socket.write(buildLoginPacket("__XximyanicexX__", 0x00)); // harmful >3
      return;
      setTimeout(() => {
        console.log("sending location (0x02) packet (sf map 5)");
        socket.write(buildLocationPacket(true, 5));
      }, 1000);

      setTimeout(() => {
        console.log("sending bad packet");
        socket.write(buildErrorTestPacket());
      }, 2000);
    },
    close() {
      console.log("closed 💔");
    },
    error(socket, error) {
      console.error("nuh uh:", error);
    },
  },
});
