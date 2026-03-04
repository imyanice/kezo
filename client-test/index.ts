const socket = await Bun.connect({
	hostname: 'localhost',
	port: 8080,

	socket: {
		data(socket, data) {},
		open(socket) {},
		close(socket, error) {},
		drain(socket) {},
		error(socket, error) {},

		// client-specific handlers
		connectError(socket, error) {}, // connection failed
		end(socket) {}, // connection closed by server
		timeout(socket) {}, // connection timed out
	},
})

const msg = 'hello wold'
const body = new TextEncoder().encode(msg)

if (body.length > 255) {
	throw new Error('Message too long for 1-byte length prefix')
}

const packet = new Uint8Array(1 + body.length)
packet[0] = body.length // 1-byte size prefix
packet.set(body, 1) // payload after size

socket.write(packet)
//socket.write("!hello")
