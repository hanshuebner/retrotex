const Koa = require("koa");
const Router = require("koa-router");
const { koaBody } = require("koa-body");
const fs = require("fs");
const path = require("path");
const WebSocket = require("ws");
const serve = require("koa-static");

const app = new Koa();
const router = new Router();
const wss = new WebSocket.Server({ noServer: true });

let currentFile = "default.cept";

// Function to read and send CEPT data
const sendCeptData = (ws, filename) => {
  const filePath = path.join(__dirname, "cept_files", filename);
  const readStream = fs.createReadStream(filePath);
  readStream.on("data", (chunk) => {
    ws.send(chunk);
  });
  readStream.on("error", (err) => {
    console.error("Error reading file:", err);
    ws.close();
  });
};

// WebSocket connection handling
wss.on("connection", (ws) => {
  sendCeptData(ws, currentFile);
});

// POST endpoint to select a file
router.post("/api/get-cept-file/:filename", async (ctx) => {
  const { filename } = ctx.params;
  const filePath = path.join(__dirname, "cept_files", filename);
  if (fs.existsSync(filePath)) {
    currentFile = filename;
    ctx.status = 200;
    ctx.body = { message: "File selected successfully" };
  } else {
    ctx.status = 404;
    ctx.body = { message: "File not found" };
  }
});

app.use(koaBody());
app.use(router.routes());
app.use(router.allowedMethods());
app.use(serve(path.join(__dirname, "../client")));

const server = app.listen(3000, () => {
  console.log("Server running on http://localhost:3000");
});

server.on("upgrade", (request, socket, head) => {
  wss.handleUpgrade(request, socket, head, (ws) => {
    wss.emit("connection", ws, request);
  });
});
