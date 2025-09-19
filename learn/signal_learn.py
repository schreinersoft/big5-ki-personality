import signal
import sys
import time

def signal_handler(sig, frame):
    print('\nYou pressed Ctrl+C! Cleaning up...')
    print(frame.f_locals.get("a"))
    print(frame.f_locals.get("b"))
    
    print('Cleanup completed. Goodbye!')
    sys.exit(0)

# Register the signal handler
signal.signal(signal.SIGINT, signal_handler)

print("Running... Press Ctrl+C to stop")
try:
    a= 20
    while True:
        print("Working...")
        time.sleep(1)
except Exception as e:
    print(f"Unexpected error: {e}")