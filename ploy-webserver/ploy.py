import http
from http.server import BaseHTTPRequestHandler, HTTPServer
import json
import subprocess
import os.path
import pathlib

# Settings
hostName = "127.0.0.1"
serverPort = 8000
# if relative path (without starting "/"), then relative from home directory
# if absolute path (with starting "/"), then absolute path
PATH_TO_BOT = "PATH_TO_EXECUTABLE"
PATH = str(pathlib.Path.home()/PATH_TO_BOT)

# Server Implementation
class PloyServer(BaseHTTPRequestHandler):

    game = {
        "validation": False,
        "bot": False,
        "status": "",
        "info": "",
        "players": ["You", "You"],
        "turn": 0,
        "yourturn": True,
        "board": ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,",
        "construct": ""
    }

    def buildCell(self, str):
        if str == "":
            return "Empty"
        else:
            player = str[0]
            if player == 'w':
                player = "White"
            else:
                player = "Black"
            o = str[1:]
            return "(Piece " + player + " " + o + ")"

    def constructBoard(self, fen):
        newSplit = list(map(lambda s : s.split(","), fen.split("/")))
        newRows = [list(map(self.buildCell, row)) for row in newSplit]
        rows = [",".join(x) for x in newRows]
        build = "[[" + ("],[".join(rows)) + "]]"    
        return build

    def sendBoard(self):
        self.game["construct"] = self.constructBoard(self.game["board"])
        self.send_response(200)
        self.send_header("Content-type", "text/html")
        self.end_headers()
        self.wfile.write(bytes(json.dumps(self.game), "utf-8"))

    def startNewGame(self):
        self.game["info"] = ""
        self.game["status"] = "Move Black"
        if not self.game["bot"]:
            self.game["players"] = ["You", "You"]
        else:
            self.game["players"] = ["You", "Bot"]
        self.game["turn"] = 0
        self.game["yourturn"] = True,
        self.game["board"] = ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,"
        if self.game["validation"]:
            if self.checkExecutable():
                self.game["info"] = "The executable bot seems to be integrated."
            else:
                self.game["info"] = "The executable bot DOES NOT work."

        self.sendBoard()

    def tran(self, fld):
        colF = fld[0]
        rowF = fld[1]
        return (9 - int(rowF), ord(colF) - 97)

    def rotate(self, startPiece, rt):
        player = startPiece[0]
        orientation = int(startPiece[1:])
        new = ((orientation << rt) | ((orientation >> (8-rt)))) & 255   # TODO:
        return player + str(new)

    def calcFEN(self, old, start, target, rotation):
        newSplit = list(map(lambda s : s.split(","), old.split("/")))
        (rowS, colS) = self.tran(start)
        (rowT, colT) = self.tran(target)
        startPiece = newSplit[rowS][colS]
        if startPiece == "":
            return old
        else:
            rotatedPiece = self.rotate(startPiece, int(rotation))
            newSplit[rowS][colS] = ""
            newSplit[rowT][colT] = rotatedPiece
            rows = [",".join(x) for x in newSplit]
            fen = "/".join(rows)
            return fen

    def checkExecutable(self):
        if not os.path.isfile(PATH):
            return False
        else:
            moves = self.execute(
                ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,",
                "w",
                "-all")
            if moves == "[]":
                return True
            else:
                return False


    def execute(self, fen, player, param=""):
        cmd = PATH
        args = [cmd, fen, player, param]
        print(' '.join(args))
        completed_process = subprocess.run(args, capture_output=True, encoding='utf8')
        output = completed_process.stdout.strip()
        return output


    def allMoves(self):
        if self.game["turn"] == 0:
            player = "b"
        else:
            player = "w"
        fen = self.game["board"]
        allMoves = self.execute(fen, player, "-all")
        return allMoves[1:len(allMoves)-1].split(",")


    def singleMove(self):
        if self.game["turn"] == 0:
            player = "b"
        else:
            player = "w"
        fen = self.game["board"]
        move = self.execute(fen, player)
        return move


    def playBot(self):
        move = self.singleMove()
        # TODO: if no move, then finished
        if (move == ""):
            self.game["info"] = "No move possible, game finished?"
        else:
            moveArr = move.split("-")
            self.doMove(moveArr[0], moveArr[1], moveArr[2])


    def doMove(self, start, target, rotation):
        valid = True
        finished = False
        if self.game["validation"]:
            moves = self.allMoves()
            finished = (not moves)
            move = start + "-" + target + "-" + rotation
            valid = (move in moves)
        if (valid & (not finished)):
            self.game["info"] = ""
            self.game["status"] = ""
            turn = (self.game["turn"] + 1) % 2
            self.game["turn"] = turn
            if turn == 0:
                self.game["status"] = "Move Black"
            else:
                self.game["status"] = "Move White"
            self.game["yourturn"] = (not self.game["bot"]) | (turn == 0)
            self.game["board"] = self.calcFEN(self.game["board"], start, target, rotation)
        elif finished:
            self.game["info"] = "No move possible, game finished?"
        else:
            self.game["info"] = "Move not valid - try other move." 
        self.sendBoard()

        if self.game["bot"] & self.game["turn"] == 1:
            self.playBot()


    def gameLogic(self):
        args = self.path.split("?")[1]
        if args == 'usecase=start-bot':
            self.game["validation"] = True
            self.game["bot"] = True
            self.startNewGame()
        elif args == 'usecase=start-validation':
            self.game["validation"] = True
            self.game["bot"] = False
            self.startNewGame()
        elif args == 'usecase=start-novalidation':
            self.game["validation"] = False
            self.game["bot"] = False
            self.startNewGame()
        elif args == ("get"):
            self.sendBoard()
        elif args.startswith("usecase=trymove-"):
            i = 3
            splitted = args.split("-")
            start = splitted[1]
            target = splitted[2]
            rotation = splitted[3]
            self.doMove(start, target, rotation)
        else:
            self.send_response(200)
            self.send_header("Content-type", "application/json")
            self.end_headers()


    def determineContentType(self, filename):
        if filename.endswith(".css"):
            return "text/css"
        elif filename.endswith(".png"):
            return "image/jpeg"
        elif filename.endswith(".js"):
            return "text/javascript"
        else:
            return "text/html"


    def openFile(self, fileName):
        try:
            with open(fileName, "rb") as file:
                self.send_response(200)
                self.send_header("Content-type", self.determineContentType(fileName))
                self.end_headers()
                self.wfile.write(file.read())
        except FileNotFoundError:
            self.send_response(http.HTTPStatus.NOT_FOUND)


    def do_GET(self):
        path = self.path.split("?")[0]
        if path == '/':
            self.openFile("index.html")
        elif path == '/ploy':
            self.openFile("ploy.html")
        elif path == '/logic':
            self.gameLogic()
        else:
            self.openFile(self.path[1:])


if __name__ == "__main__":        
    webServer = HTTPServer((hostName, serverPort), PloyServer)
    print("Server started http://%s:%s" % (hostName, serverPort))
    try:
        webServer.serve_forever()
    except KeyboardInterrupt:
        pass

    webServer.server_close()
    print("Server stopped.")