import SwiftUI
import GhosttyKit

@main
struct Ghostty_iOSApp: App {
    @StateObject private var ghostty_app: Ghostty.App

    init() {
        if ghostty_init(UInt(CommandLine.argc), CommandLine.unsafeArgv) != GHOSTTY_SUCCESS {
            preconditionFailure("Initialize ghostty backend failed")
        }
        _ghostty_app = StateObject(wrappedValue: Ghostty.App())
    }

    var body: some Scene {
        WindowGroup {
            iOS_CustomIOTerminal()
                .environmentObject(ghostty_app)
        }
    }
}

/// Showcases custom I/O: the terminal uses the callback backend instead of a pty.
/// Keyboard input is echoed back to the terminal, demonstrating the full round-trip.
struct iOS_CustomIOTerminal: View {
    @EnvironmentObject private var ghostty_app: Ghostty.App

    var body: some View {
        ZStack {
            Color(ghostty_app.config.backgroundColor).ignoresSafeArea()

            if let app = ghostty_app.app {
                CustomIOSurface(app: app)
            }
        }
    }
}

/// Manages a Ghostty surface with custom I/O callbacks.
struct CustomIOSurface: View {
    let app: ghostty_app_t

    @StateObject private var surfaceView: Ghostty.SurfaceView

    init(app: ghostty_app_t) {
        self.app = app
        var config = Ghostty.SurfaceConfiguration()
        config.useCustomIO = true
        _surfaceView = StateObject(wrappedValue: Ghostty.SurfaceView(app, baseConfig: config))
    }

    var body: some View {
        Ghostty.SurfaceWrapper(surfaceView: surfaceView)
            .onAppear {
                setupCustomIO()
            }
    }

    private func setupCustomIO() {
        guard let surface = surfaceView.surface else { return }

        // Set up the write callback: when the user types, echo it back to the terminal.
        // The userdata is the raw pointer to the surface.
        let surfacePtr = UnsafeMutableRawPointer(mutating: surface)
        ghostty_surface_set_write_callback(surface, { userdata, data, len in
            guard let userdata = userdata, let data = data, len > 0 else { return }
            let surface = ghostty_surface_t(userdata)

            // Simple local echo: feed the typed data back to the terminal for display.
            // In a real app this would go to an SSH channel, serial port, etc.
            // Handle CR -> CRLF for proper terminal display.
            let slice = UnsafeBufferPointer(start: data, count: len)
            var output = Data()
            for byte in slice {
                if byte == 0x0D { // CR
                    output.append(contentsOf: [0x0D, 0x0A]) // CRLF
                } else {
                    output.append(byte)
                }
            }
            output.withUnsafeBytes { buf in
                ghostty_surface_feed_data(surface, buf.baseAddress!.assumingMemoryBound(to: UInt8.self), buf.count)
            }
        }, surfacePtr)

        // Feed a welcome banner to show custom I/O is working.
        let welcome = """
        \u{1b}[1;32mGhostty Custom I/O Demo\u{1b}[0m\r\n\
        \r\n\
        This terminal uses the callback backend.\r\n\
        There is no shell -- keyboard input is echoed back directly.\r\n\
        \r\n\
        In a real app, you would connect this to an SSH channel,\r\n\
        serial port, WebSocket, or any other data source.\r\n\
        \r\n\
        \u{1b}[1;36mType anything to see it echoed:\u{1b}[0m \r\n
        """
        let data = Array(welcome.utf8)
        data.withUnsafeBufferPointer { buf in
            ghostty_surface_feed_data(surface, buf.baseAddress!, buf.count)
        }
    }
}
