import SwiftUI
import Foundation
import GhosttyKit
import IOSurface

@_silgen_name("ghostty_surface_feed_data")
private func ghostty_surface_feed_data(_ surface: ghostty_surface_t, _ data: UnsafePointer<UInt8>, _ len: Int)

#if DEBUG
private func ghosttyDebugFeed(surface: ghostty_surface_t) {
    let text = "Ghostty iOS debug feed\\r\\n$ "
    let bytes = Array(text.utf8)
    bytes.withUnsafeBufferPointer { buf in
        guard let base = buf.baseAddress else { return }
        ghostty_surface_feed_data(surface, base, buf.count)
    }
    ghostty_surface_refresh(surface)
    ghostty_surface_draw(surface)
}
#endif

extension Ghostty {
    /// The UIView implementation for a terminal surface.
    class SurfaceView: UIView, ObservableObject {
        typealias ID = UUID

        /// Unique ID per surface
        let id: UUID

        // The current title of the surface as defined by the pty. This can be
        // changed with escape codes. This is public because the callbacks go
        // to the app level and it is set from there.
        @Published var title: String = "👻"

        // The current pwd of the surface.
        @Published var pwd: String? = nil

        // The cell size of this surface. This is set by the core when the
        // surface is first created and any time the cell size changes (i.e.
        // when the font size changes). This is used to allow windows to be
        // resized in discrete steps of a single cell.
        @Published var cellSize: OSSize = .zero

        // The health state of the surface. This currently only reflects the
        // renderer health. In the future we may want to make this an enum.
        @Published var healthy: Bool = true

        // Any error while initializing the surface.
        @Published var error: Error? = nil

        // The hovered URL
        @Published var hoverUrl: String? = nil
        
        // The progress report (if any)
        @Published var progressReport: Action.ProgressReport? = nil

        // The time this surface last became focused. This is a ContinuousClock.Instant
        // on supported platforms.
        @Published var focusInstant: ContinuousClock.Instant? = nil

        /// True when the bell is active. This is set inactive on focus or event.
        @Published var bell: Bool = false
        
        // The current search state. When non-nil, the search overlay should be shown.
        @Published var searchState: SearchState? = nil

        // The currently active key tables. Empty if no tables are active.
        @Published var keyTables: [String] = []

        /// True when the surface is in readonly mode.
        @Published private(set) var readonly: Bool = false
        
        /// True when the surface should show a highlight effect (e.g., when presented via goto_split).
        @Published private(set) var highlighted: Bool = false

        // Returns sizing information for the surface. This is the raw C
        // structure because I'm lazy.
        var surfaceSize: ghostty_surface_size_s? {
            guard let surface = self.surface else { return nil }
            return ghostty_surface_size(surface)
        }

        private(set) var surface: ghostty_surface_t?

        init(_ app: ghostty_app_t, baseConfig: SurfaceConfiguration? = nil, uuid: UUID? = nil) {
            self.id = uuid ?? .init()

            // Initialize with some default frame size. The important thing is that this
            // is non-zero so that our layer bounds are non-zero so that our renderer
            // can do SOMETHING.
            super.init(frame: CGRect(x: 0, y: 0, width: 800, height: 600))
            self.contentScaleFactor = UIScreen.main.scale

            // Setup our surface. This will also initialize all the terminal IO.
            let surface_cfg = baseConfig ?? SurfaceConfiguration()
            let surface = surface_cfg.withCValue(view: self) { surface_cfg_c in
                ghostty_surface_new(app, &surface_cfg_c)
            }
            guard let surface = surface else {
                // TODO
                return
            }
            self.surface = surface;

#if DEBUG
            if ProcessInfo.processInfo.environment["GHOSTTY_DEBUG_FEED"] == "1" {
                let surface_ref = surface
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.2) {
                    print("[SurfaceView] debug feed injecting")
                    ghosttyDebugFeed(surface: surface_ref)
                }
            }
#endif
        }

        required init?(coder: NSCoder) {
            fatalError("init(coder:) is not supported for this view")
        }

        deinit {
            guard let surface = self.surface else { return }
            ghostty_surface_free(surface)
        }

        func focusDidChange(_ focused: Bool) {
            guard let surface = self.surface else { return }
            ghostty_surface_set_focus(surface, focused)

            // On macOS 13+ we can store our continuous clock...
            if (focused) {
                focusInstant = ContinuousClock.now
            }
        }

        func sizeDidChange(_ size: CGSize) {
            guard let surface = self.surface else { return }

            // Ghostty wants to know the actual framebuffer size... It is very important
            // here that we use "size" and NOT the view frame. If we're in the middle of
            // an animation (i.e. a fullscreen animation), the frame will not yet be updated.
            // The size represents our final size we're going for.
            let scale = self.contentScaleFactor
            ghostty_surface_set_content_scale(surface, scale, scale)
            ghostty_surface_set_size(
                surface,
                UInt32(size.width * scale),
                UInt32(size.height * scale)
            )

            // Force a draw on iOS to ensure IOSurface contents are produced.
            print("[SurfaceView] sizeDidChange -> refresh/draw size=\(size) scale=\(scale)")
            ghostty_surface_refresh(surface)
            ghostty_surface_draw(surface)
        }

        // MARK: UIView

        // Use default CALayer; Ghostty adds its own IOSurfaceLayer sublayer.
        override class var layerClass: AnyClass {
            CALayer.self
        }

        override func didMoveToWindow() {
            sizeDidChange(frame.size)
        }

        override func layoutSubviews() {
            super.layoutSubviews()
            // Update sublayer frames to match view bounds
            print("[SurfaceView] layoutSubviews: bounds=\(bounds), sublayers=\(layer.sublayers?.count ?? 0)")
            if let sublayers = layer.sublayers {
                for (i, sublayer) in sublayers.enumerated() {
                    let scale = contentScaleFactor
                    print("[SurfaceView] sublayer[\(i)] before: frame=\(sublayer.frame), bounds=\(sublayer.bounds)")
                    sublayer.frame = bounds
                    sublayer.contentsScale = contentScaleFactor
                    print("[SurfaceView] sublayer[\(i)] after: frame=\(sublayer.frame)")
                    let className = String(describing: type(of: sublayer))
                    let expectedW = sublayer.bounds.width * scale
                    let expectedH = sublayer.bounds.height * scale
                    print("[SurfaceView] sublayer[\(i)] class=\(className) scale=\(sublayer.contentsScale) expectedPx=\(expectedW)x\(expectedH)")
                    if let contents = sublayer.contents, let ioSurface = contents as? IOSurface {
                        let w = IOSurfaceGetWidth(ioSurface)
                        let h = IOSurfaceGetHeight(ioSurface)
                        let bpr = IOSurfaceGetBytesPerRow(ioSurface)
                        let pf = IOSurfaceGetPixelFormat(ioSurface)
                        print("[SurfaceView] iosurface[\(i)]: \(w)x\(h) bpr=\(bpr) pf=\(pf)")
                    } else {
                        print("[SurfaceView] iosurface[\(i)]: no contents")
                    }
                }
            }
            sizeDidChange(bounds.size)

            if let surface = surface {
                print("[SurfaceView] layoutSubviews -> refresh/draw bounds=\(bounds) scale=\(contentScaleFactor)")
                ghostty_surface_refresh(surface)
                ghostty_surface_draw(surface)
            }
        }
    }
}
